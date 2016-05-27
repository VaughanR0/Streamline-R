forecast.gts <- function(object, h = ifelse(frequency(object$bts) > 1L,
                         2L * frequency(object$bts), 10L), 
                         method = c("comb", "bu", "mo", 
                                    "tdgsa", "tdgsf", "tdfp"),
                         fmethod = c("ets", "arima", "rw"), 
                         algorithms = c("lu", "cg", "chol", "recursive", "slm"),
                         keep.fitted = FALSE, keep.resid = FALSE,
                         keep.model = FALSE, keep.intervals = FALSE,
						 do.seasonal = FALSE,
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
  #   do.seasonal: Allow use of lower level seasonal models even if top level is not seasonal
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

  # Create global variable for seasonality of total level
  # This does not work too well, ideally would use the results of model
  # but dont want to do that work twice
  seasfn <- function(xall, n, i, ...) {  
	# print(paste("index is", i))
    x <- xall[,i]
	nme <- n[[i]]
	if (nme == "Total" && !do.seasonal) {
		# define this variable in the global environment (not pretty)
		# finds the frequency period of the data using spectral methods,
		# if it is greater than 1 it has some frequency, 12 would be a monthly frequency
		level0.freq <<- as.numeric(findfrequency(x))
		print(paste("Frequency of data at Level 0 is", level0.freq))
	}
  }
  # loop function to grab pf, fitted, resid
  loopfn <- function(x, ...) {  
    out <- list()
    if (is.null(FUN)) {
      if (fmethod == "ets") {
		# Disallow seasonal models if level 0 is not seasonal
		modelspec <- ifelse(level0.freq == 1, "ZZN", "ZZZ")
	    print(paste("Loopfn: using ets: model specification", modelspec))
        models <- ets(x, model=modelspec, lambda = lambda, ...)
		if (keep.intervals) {
		  fc <- forecast(models, h= h)
		} else {
		  fc <- forecast(models, h=h, PI=FALSE)
		}
      } else if (fmethod == "arima") {
		allow.seas <- ifelse(level0.freq == 1, FALSE, TRUE)
	    print(paste("Loopfn: using auto.arima: allow seasonal", allow.seas))
        models <- auto.arima(x, seasonal=allow.seas, lambda = lambda, xreg = xreg, 
                             parallel = FALSE, ...)
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
      out$model <- models
    }
    if (keep.intervals) {
	  # These should really be weighted by the resulting method factors
      out$upper <- fc$upper
      out$lower <- fc$lower
    }

    return(out)
  }

  if (parallel) { # parallel == TRUE
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    # Parallel start new process
    lambda <- lambda
    xreg <- xreg
    newxreg <- newxreg
    cl <- makeCluster(num.cores)
    loopout <- parSapplyLB(cl = cl, X = y, FUN = function(x) loopfn(x, ...), 
                           simplify = FALSE)
    stopCluster(cl = cl)
  } else {  # parallel = FALSE
	lapply(seq(to=ncol(y)), seasfn, xall=y, n=colnames(y))
	# Calls the loopfn as an anonymous function for each vector in y
	# and returns a list
    # Could just be as loopfn is defined above
	# loopout <- lapply(y, loopfn)
    loopout <- lapply(y, function(x) loopfn(x, ...))
  }

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

    if (weights == "none") {
      bfcasts <- Comb(pfcasts, keep = "bottom", algorithms = alg)
    } else if (any(weights == c("sd", "nseries"))) {
      bfcasts <- Comb(pfcasts, weights = wvec, keep = "bottom", 
                      algorithms = alg)
    } 
      if (weights == "none") {
        fits <- Comb(fits, keep = "bottom", algorithms = alg)
      } else if (any(weights == c("sd", "nseries"))) {
        fits <- Comb(fits, weights = wvec, keep = "bottom",
                     algorithms = alg)
      } 
    }
      if (weights == "none") {
        resid <- Comb(resid, keep = "bottom", algorithms = alg)
      } else if (any(weights == c("sd", "nseries"))) {
        resid <- Comb(resid, weights = wvec, keep = "bottom",
                      algorithms = alg)
      } 
    }
      if (weights == "none") {
        upper <- Comb(upper, keep = "bottom", algorithms = alg)
        lower <- Comb(lower, keep = "bottom", algorithms = alg)
      } else if (any(weights == c("sd", "nseries"))) {
        upper <- Comb(upper, weights = wvec, keep = "bottom", algorithms = alg)
        lower <- Comb(lower, weights = wvec, keep = "bottom", algorithms = alg)
      } 
    }
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

  bfcasts <- ts(bfcasts, start = tsp.y[2L] + 1L/tsp.y[3L], 
                frequency = tsp.y[3L])
  colnames(bfcasts) <- bnames
  class(bfcasts) <- class(object$bts)
  attr(bfcasts, "msts") <- attr(object$bts, "msts")

  if (keep.fitted) {
    bfits <- ts(fits, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bfits) <- bnames
  } 
  if (keep.resid) {
    bresid <- ts(resid, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bresid) <- bnames
  }
  if (keep.intervals) {
    bupper <- ts(upper, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(bupper) <- bnames
    blower <- ts(lower, start = tsp.y[1L], frequency = tsp.y[3L])
    colnames(blower) <- bnames
  }

  # Output
  out <- list(bts = bfcasts, histy = object$bts, labels = object$labels,
              method = method, fmethod = fmethod)
  if (keep.fitted0) {
    out$fitted <- bfits
  }
  if (keep.resid) {
    out$residuals <- bresid
  }
    if (keep.model) {
      out$model <- model
    }
    if (keep.intervals) {
	  # These should really be weighted by the resulting method factors
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
