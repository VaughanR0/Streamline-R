forecast.gts <- function(object, h = ifelse(frequency(object$bts) > 1L,
                         2L * frequency(object$bts), 10L), 
                         method = c("comb", "bu", "mo", 
                                    "tdgsa", "tdgsf", "tdfp"),
                         fmethod = c("ets", "arima", "rw"), 
                         algorithms = c("lu", "cg", "chol", "recursive", "slm"),
                         keep.fitted = FALSE, keep.resid = FALSE,
                         keep.model = FALSE, keep.intervals = FALSE,
                         do.season = FALSE,
                         positive = FALSE, lambda = NULL, level, 
                         weights = c("sd", "none", "nseries"),
                         parallel = FALSE, num.cores = 2, FUN = NULL,
                         xreg = NULL, newxreg = NULL, ...) {
  # Forecast hts or gts objects
  #
  # Args:
  #   object*: Only hts/gts can be passed onto this function.
  #   h: h-step forecasts.
  #   method: Aggregated approaches.
  #   fmethod: Forecast methods.
  #   keep: Users specify what they'd like to keep at the bottom level.
  #   do.season: Allow use of lower level seasonal models even if top level is not seasonal
  #   positive & lambda: Use Box-Cox transformation.
  #   level: Specify level for the middle-out approach, starting with level 0.
  #
  # Return:
  #   Point forecasts with other info chosen by the user.
  method <- match.arg(method)
  weights <- match.arg(weights)
  alg <- match.arg(algorithms)
  if (is.null(FUN)) {
    fmethod <- match.arg(fmethod)
  }
  # Error Handling:
  if (!is.gts(object)) {
    stop("Argument object must be either a hts or gts object.")
  }
  if (h < 1L) {
    stop("Argument h must be positive.")
  }
  if (!is.hts(object) && 
      is.element(method, c("mo", "tdgsf", "tdgsa", "tdfp"))) {
    stop("Argument method is not appropriate for a non-hierarchical time series.")
  }
  if (method == "mo" && missing(level)) {
    stop("Please specify argument level for the middle-out method.")
  }

  # Set up lambda for arg "positive" when lambda is missing
  if (is.null(lambda)) {
    if (positive) {
      if (any(object$bts <= 0L)) {
        stop("All data must be positive.")
      } else {
        lambda <- 0
      }
    } else {
      lambda <- NULL
    }
  }

  # Remember the original keep.fitted argument for later
  keep.fitted0 <- keep.fitted
  if (method == "comb" && weights == "sd") {
    keep.fitted <- TRUE
  }

  # Set up "level" for middle-out
  if (method == "mo") {
    len <- length(object$nodes)
    if (level < 0L || level > len) {
      stop("Argument level is out of the range.")
    } else if (level == 0L) {
      method <- "tdfp"
    } else if (level == len) {
      method <- "bu"
    } else {
      mo.nodes <- object$nodes[level:len]
      level <- seq(level, len)
    }
  }

  # Set up forecast methods
  if (any(method == c("comb", "tdfp"))) { # Combination or tdfp
    y <- aggts(object)  # Grab all ts
  } else if (method == "bu") {  # Bottom-up approach
    y <- object$bts  # Only grab the bts
  } else if (any(method == c("tdgsa", "tdgsf")) && method != "tdfp") {
    y <- aggts(object, levels = 0)  # Grab the top ts
  } else if (method == "mo") {
    y <- aggts(object, levels = level)
  }
  allnames <- colnames(y)
  level0.seas <- TRUE

  # Different version of loopfn which can determine when we are doing level0
  # which enables special treatment of seasonality at lower levels
  seasfn <- function(xall, n, i, ...) {
    out <- list()
    x <- xall[,i]
    if (n[[i]] == "Total") {
      level0 <- TRUE
    } else {
      level0 <- FALSE
    }
    if (is.null(FUN)) {
      if (fmethod == "ets") {
        # Disallow seasonal models if level 0 is not seasonal
        if (level0.seas) {
          modelspec <- "ZZZ"
        } else {
          modelspec <- "ZZN"
        }
        # print(paste("seasfn: using ets: model specification", modelspec))
        models <- ets(x, model=modelspec, lambda = lambda, ...)
        if (level0) {
          level0.seas <<- isSeasonal(models, "ets")
          print(paste("seasfn: ets: is seasonal at top level returned", level0.seas))
        }
        if (keep.intervals) {
          fc <- forecast(models, h= h)
        } else {
          fc <- forecast(models, h=h, PI=FALSE)
        }
      } else if (fmethod == "arima") {
        if (level0) {
          models <- auto.arima(x, lambda = lambda, xreg = xreg, parallel = FALSE, ...)
          level0.seas <<- isSeasonal(models, "arima")
          print(paste("seasfn: arima: is seasonal at top level returned", level0.seas))
        } else {
          # This works as they are all logicals
          allow.seas <- ifelse(level0.seas, TRUE, FALSE)
          # print(paste("seasfn: using auto.arima: allow seasonal", allow.seas))
          models <- auto.arima(x, seasonal=allow.seas, lambda = lambda, xreg = xreg, parallel = FALSE, ...)
        }
        fc <- forecast(models, h = h, xreg = newxreg)
      } else if (fmethod == "rw") {
	    # random walk method
        fc <- rwf(x, h = h, lambda = lambda, ...)
      }
    } else { # user defined function to produce point forecasts
      models <- FUN(x, ...)
      fc <- forecast(models, h = h)
    }
    out$pfcasts <- fc$mean
    if (keep.fitted) {
      out$fitted <- fitted(models)
    }
    if (keep.resid) {
      out$resid <- residuals(models)
    }
    if (keep.model) {
      out$model <- stdModel(models,fmethod)
    }
    if (keep.intervals) {
      out$upper <- fc$upper
      out$lower <- fc$lower
    }
    return(out)
  }
  # loop function to grab pf, fitted, resid
  loopfn <- function(x, ...) {
    out <- list()
    if (is.null(FUN)) {
      if (fmethod == "ets") {
        models <- ets(x, lambda = lambda, ...)
        if (keep.intervals) {
          fc <- forecast(models, h= h)
        } else {
          fc <- forecast(models, h=h, PI=FALSE)
        }
      } else if (fmethod == "arima") {
        models <- auto.arima(x, lambda = lambda, xreg = xreg, parallel = FALSE, ...)
        fc <- forecast(models, h = h, xreg = newxreg)
      } else if (fmethod == "rw") {
        fc <- rwf(x, h = h, lambda = lambda, ...)
      }
    } else { # user defined function to produce point forecasts
      models <- FUN(x, ...)
      fc <- forecast(models, h = h)
    }
    out$pfcasts <- fc$mean
    if (keep.fitted) {
      out$fitted <- fitted(models)
    }
    if (keep.resid) {
      out$resid <- residuals(models)
    }
    if (keep.model) {
      out$model <- stdModel(models,fmethod)
    }
    if (keep.intervals) {
      out$upper <- fc$upper
      out$lower <- fc$lower
    }
    return(out)
  }

  # require('parallel')
  if (parallel) { # parallel == TRUE
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    # Parallel start new process
    lambda <- lambda
    xreg <- xreg
    newxreg <- newxreg
    cl <- makeCluster(num.cores)
    if (do.season) {
		# do seasonal at any level
		loopout <- parSapplyLB(cl=cl, X=y, FUN=function(x) loopfn(x, ...), simplify=FALSE)
    } else {
		# only do seasonal at other levels if top level is seasonal
		# see William Dunlap http://r.789695.n4.nabble.com/Parallel-computing-how-to-transmit-multiple-parameters-to-a-function-in-parLapply-td4682667.html
		# withGlobals <- function(FUN, ...){ environment(FUN) <- list2env(list(...)) FUN } 
		# loopout <- parSapplyLB(cl=cl, X=seq(to=ncol(y)), withGlobals(seasfn, xall=y, n=colnames(y)), simplify = FALSE)
		# or possibly
		# https://stackoverflow.com/questions/47346810/parsapply-with-2-arguments
		loopout <- parSapplyLB(cl=cl,X=seq(to=ncol(y)),FUN=seasfn,xall=y,n=colnames(y), simplify=FALSE)
		# non-parallel
		# loopout <- lapply(seq(to=ncol(y)), seasfn, xall=y, n=colnames(y))
	}
    stopCluster(cl = cl)
  } else {  # parallel = FALSE
    if (do.season) {
      # do seasonal at any level
      loopout <- lapply(y, function(x) loopfn(x, ...))
    } else {
      # only do seasonal at other levels if top level is seasonal
      loopout <- lapply(seq(to=ncol(y)), seasfn, xall=y, n=colnames(y))
    }
  }

	pifun <- function(x) {
	  nr2 <- nrow(x)
	  nr <- nr2 %/% 2
	  nr1 <- nr+1
	  x1 <- x[1:nr,]
	  x2 <- x[nr1:nr2,]
	  out <- cbind(x1,x2)
	  return(out)
	}

  # Has all levels here
  pfcasts <- sapply(loopout, function(x) x$pfcasts)
  if (keep.fitted) {
    fits <- sapply(loopout, function(x) x$fitted)
  }
  if (keep.resid) {
    resid <- sapply(loopout, function(x) x$resid)
  }
  if (keep.model) {
    model <- sapply(loopout, function(x) x$model)
  }
  if (keep.intervals) {
    upper <- sapply(loopout, function(x) x$upper)
    lower <- sapply(loopout, function(x) x$lower)
  }

  if (is.vector(pfcasts)) {  # if h = 1, sapply returns a vector
    pfcasts <- t(pfcasts)
  }

  # Set up basic info
  tsp.y <- tsp(y)
  bnames <- colnames(object$bts)

  if (method == "comb") { # Assign class
    class(pfcasts) <- class(object)
    if (keep.fitted) {
      class(fits) <- class(object)
    }
    if (keep.resid) {
      class(resid) <- class(object)
    }
    if (keep.intervals) {
      class(upper) <- class(object)
      class(lower) <- class(object)
    }
    if (weights == "nseries") {
      if (is.hts(object)) {
        wvec <- InvS4h(object$nodes)
      } else {
        wvec <- InvS4g(object$groups)
      }
    } else if (weights == "sd") {
      tmp.resid <- y - fits # it ensures resids are additive errors
      wvec <- 1/sqrt(colMeans(tmp.resid^2, na.rm = TRUE))
    }
  }

  # An internal function to call combinef correctly
  Comb <- function(x, ...) {
    if (is.hts(x)) {
      return(combinef(x, nodes = object$nodes, ... ))
    } else {
      return(combinef(x, groups = object$groups, ...))
    }
  }

  if (method == "comb") {
    if (weights == "none") {
      bfcasts <- Comb(pfcasts, keep = "all", algorithms = alg)
    } else if (any(weights == c("sd", "nseries"))) {
      bfcasts <- Comb(pfcasts, weights = wvec, keep = "all", 
                      algorithms = alg)
    } 
  if (keep.fitted) {
      if (weights == "none") {
        fits <- Comb(fits, keep = "all", algorithms = alg)
      } else if (any(weights == c("sd", "nseries"))) {
        fits <- Comb(fits, weights = wvec, keep = "all",
                     algorithms = alg)
      } 
    }
  if (keep.resid) {
      if (weights == "none") {
        resid <- Comb(resid, keep = "all", algorithms = alg)
      } else if (any(weights == c("sd", "nseries"))) {
        resid <- Comb(resid, weights = wvec, keep = "all",
                      algorithms = alg)
      } 
    }
  if (keep.intervals) {
      if (weights == "none") {
        upper <- Comb(upper, keep = "all", algorithms = alg)
        lower <- Comb(lower, keep = "all", algorithms = alg)
      } else if (any(weights == c("sd", "nseries"))) {
        upper <- Comb(upper, weights = wvec, keep = "all", algorithms = alg)
        lower <- Comb(lower, weights = wvec, keep = "all", algorithms = alg)
      } 
    }
  } else if (method == "bu") {
    bfcasts <- pfcasts
  } else if (method == "tdgsa") {
    bfcasts <- TdGsA(pfcasts, object$bts, y)
    if (keep.fitted) {
      fits <- TdGsA(fits, object$bts, y)
    }
    if (keep.resid) {
      resid <- TdGsA(resid, object$bts, y)
    }
    if (keep.intervals) {
      upper <- TdGsA(upper, object$bts, y)
      lower <- TdGsA(lower, object$bts, y)
    }
  } else if (method == "tdgsf") {
    bfcasts <- TdGsF(pfcasts, object$bts, y)
    if (keep.fitted) {
      fits <- TdGsF(fits, object$bts, y)
    }
    if (keep.resid) {
      resid <- TdGsF(resid, object$bts, y)
    }
    if (keep.intervals) {
      upper <- TdGsF(upper, object$bts, y)
      lower <- TdGsF(lower, object$bts, y)
    }
  } else if (method == "tdfp") {
    bfcasts <- TdFp(pfcasts, object$nodes)
    if (keep.fitted) {
      fits <- TdFp(fits, object$nodes)
    }
    if (keep.resid) {
      resid <- TdFp(resid, object$nodes)
    }
    if (keep.intervals) {
      upper <- TdFp(upper, object$nodes)
      lower <- TdFp(lower, object$nodes)
    }
  } else if (method == "mo") {
    bfcasts <- MiddleOut(pfcasts, mo.nodes)
    if (keep.fitted) {
      fits <- MiddleOut(fits, mo.nodes)
    }
    if (keep.resid) {
      resid <- MiddleOut(resid, mo.nodes)
    }
    if (keep.intervals) {
      upper <- MiddleOut(upper, mo.nodes)
      lower <- MiddleOut(lower, mo.nodes)
    }
  }

  # In case that accuracy.gts() is called later, since NA's have been omitted
  # to ensure slm to run without errors.
  if (method == "comb" && fmethod == "rw" 
      && keep.fitted == TRUE && !is.hts(object)) {
    fits <- rbind(rep(NA, ncol(fits)), fits)
  }

  # Convert back to time-series
  fcasts <- ts(bfcasts, start = tsp.y[2L] + 1L/tsp.y[3L], frequency = tsp.y[3L])
  colnames(fcasts) <- allnames
  class(fcasts) <- class(object$bts)
  attr(fcasts, "msts") <- attr(object$bts, "msts")

  if (keep.fitted) {
    fits <- ts(fits, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(fits) <- allnames
  } 
  if (keep.resid) {
    resid <- ts(resid, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(resid) <- allnames
  }
  if (keep.model) {
	if (fmethod != "rw") {
      colnames(model) <- allnames
    }
  }
  if (keep.intervals) {
    upper <- pifun(upper)
    upper <- ts(upper, start = tsp.y[2L] + 1L/tsp.y[3L], frequency = tsp.y[3L])
	pinames <- c(paste(allnames,".80",sep=""), paste(allnames,".95",sep=""))
    colnames(upper) <- pinames
    lower <- pifun(lower)
    lower <- ts(lower, start = tsp.y[2L] + 1L/tsp.y[3L], frequency = tsp.y[3L])
    colnames(lower) <- pinames
  }

  # Output
  if (method == "comb") {
	  # output all levels
	  out <- list(bts = fcasts, histy = y, labels = object$labels,
				  method = method, fmethod = fmethod)
  } else {
	  out <- list(bts = fcasts, histy = object$bts, labels = object$labels,
				  method = method, fmethod = fmethod)
  }
  if (keep.fitted0) {
    out$fitted <- fits
  }
  if (keep.resid) {
    out$residuals <- resid
  }
  if (keep.model) {
    out$model <- model
  }
  if (keep.intervals) {
    out$upper <- upper
    out$lower <- lower
  }

  if (is.hts(object)) {
    out$nodes <- object$nodes
  } else {
    out$groups <- object$groups
  }

  return(structure(out, class = class(object)))
}