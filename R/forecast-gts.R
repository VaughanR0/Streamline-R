#' Forecast a hierarchical or grouped time series
#'
#' Methods for forecasting hierarchical or grouped time series.
#'
#' Base methods implemented include ETS, ARIMA and the naive (random walk)
#' models. Forecasts are distributed in the hierarchy using bottom-up,
#' top-down, middle-out and optimal combination methods.
#'
#' Three top-down methods are available: the two Gross-Sohl methods and the
#' forecast-proportion approach of Hyndman, Ahmed, and Athanasopoulos (2011).
#' The "middle-out" method \code{"mo"} uses bottom-up (\code{"bu"}) for levels
#' higher than \code{level} and top-down forecast proportions (\code{"tdfp"})
#' for levels lower than \code{level}.
#'
#' For non-hierarchical grouped data, only bottom-up and combination methods
#' are possible, as any method involving top-down disaggregation requires a
#' hierarchical ordering of groups.
#'
#' When \code{xreg} and \code{newxreg} are passed, the same covariates are
#' applied to every series in the hierarchy.
#'
#' @aliases forecast.gts forecast.hts
#' @param object Hierarchical or grouped time series object of class
#' \code{{gts}}
#' @param h Forecast horizon
#' @param method Method for distributing forecasts within the hierarchy. See
#' details
#' @param weights Weights used for "optimal combination" method:
#' \code{weights="ols"} uses an unweighted combination (as described in Hyndman
#' et al 2011); \code{weights="wls"} uses weights based on forecast variances
#' (as described in Hyndman et al 2015); \code{weights="mint"} uses a full
#' covariance estimate to determine the weights (as described in Hyndman et al
#' 2016); \code{weights="nseries"} uses weights based on the number of series
#' aggregated at each node.
#' @param fmethod Forecasting method to use for each series.
#' @param algorithms An algorithm to be used for computing the combination
#' forecasts (when \code{method=="comb"}). The combination forecasts are based
#' on an ill-conditioned regression model. "lu" indicates LU decomposition is
#' used; "cg" indicates a conjugate gradient method; "chol" corresponds to a
#' Cholesky decomposition; "recursive" indicates the recursive hierarchical
#' algorithm of Hyndman et al (2015); "slm" uses sparse linear regression. Note
#' that \code{algorithms = "recursive"} and \code{algorithms = "slm"} cannot be
#' used if \code{weights="mint"}.
#' @param covariance Type of the covariance matrix to be used with
#' \code{weights="mint"}: either a shrinkage estimator (\code{"shr"}) with
#' shrinkage towards the diagonal; or a sample covariance matrix
#' (\code{"sam"}).
#' @param keep.fitted If TRUE, keep fitted values at the bottom level.
#' @param keep.resid If TRUE, keep residuals at the bottom level.
#' @param keep.intervals If TRUE, keep prediction intervals at the bottom level.
#' @param keep.model If TRUE, keep model parameters for all levels.
#' @param allow.negative If TRUE, forecasts are not truncated to 0 when negative
#' use this as setting \code{lambda=0} requires all bts to be strictly positive and
#' in my experience this is not reliable anyway - probably due to re-allocation between levels
#' @param positive If TRUE, forecasts are forced to be strictly positive (by
#' setting \code{lambda=0}).
#' @param lambda Box-Cox transformation parameter.
#' @param level Level used for "middle-out" method (only used when \code{method
#' = "mo"}).
#' @param parallel If TRUE, import \code{parallel} package to allow parallel
#' processing.
#' @param num.cores If parallel = TRUE, specify how many cores are going to be
#' used.
#' @param FUN A user-defined function that returns an object which can be
#' passed to the \code{forecast} function. It is applied to all series in order
#' to generate base forecasts.	When \code{FUN} is not \code{NULL},
#' \code{fmethod}, \code{positive} and \code{lambda} are all ignored. Suitable
#' values for \code{FUN} are \code{\link[forecast]{tbats}} and
#' \code{\link[forecast]{stlf}} for example.
#' @param xreg When \code{fmethod = "arima"}, a vector or matrix of external
#' regressors used for modelling, which must have the same number of rows as
#' the original univariate time series
#' @param newxreg When \code{fmethod = "arima"}, a vector or matrix of external
#' regressors used for forecasting, which must have the same number of rows as
#' the \code{h} forecast horizon
#' @param ... Other arguments passed to \code{\link[forecast]{ets}},
#' \code{\link[forecast]{auto.arima}} or \code{FUN}.
#' @return A forecasted hierarchical/grouped time series of class \code{gts}.
#' @author Earo Wang, Rob J Hyndman and Shanika L Wickramasuriya
#' @seealso \code{\link[hts]{hts}}, \code{\link[hts]{gts}},
#' \code{\link[hts]{plot.gts}}, \code{\link[hts]{accuracy.gts}}
#' @references G. Athanasopoulos, R. A. Ahmed and R. J. Hyndman (2009)
#' Hierarchical forecasts for Australian domestic tourism, \emph{International
#' Journal of Forecasting}, \bold{25}, 146-166.
#'
#' R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) Optimal
#' combination forecasts for hierarchical time series. \emph{Computational
#' Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
#' \url{http://robjhyndman.com/papers/hierarchical/}
#'
#' Hyndman, R. J., Lee, A., & Wang, E. (2015). Fast computation of reconciled
#' forecasts for hierarchical and grouped time series. \emph{Computational
#' Statistics and Data Analysis}, \bold{97}, 16--32.
#' \url{http://robjhyndman.com/papers/hgts/}
#'
#' Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J. (2015).
#' Forecasting hierarchical and grouped time series through trace minimization.
#' \emph{Working paper 15/15, Department of Econometrics & Business Statistics,
#' Monash University.} \url{http://robjhyndman.com/working-papers/mint/}
#'
#' Gross, C. and Sohl, J. (1990) Dissagregation methods to expedite product
#' line forecasting, \emph{Journal of Forecasting}, \bold{9}, 233-254.
#' @keywords ts
#' @method forecast gts
#' @examples
#'
#' forecast(htseg1, h = 10, method = "bu", fmethod = "arima")
#'
#' \dontrun{
#'	 forecast(
#'		 htseg2, h = 10, method = "comb", algorithms = "lu",
#'		 FUN = function(x) tbats(x, use.parallel = FALSE)
#'	 )
#' }
#'
#' @export
#' @export forecast.gts

forecast.gts <- function(
		object,
		h = ifelse(frequency(object$bts) > 1L, 2L * frequency(object$bts), 10L),
		method = c("comb", "bu", "mo", "tdgsa", "tdgsf", "tdfp"),
		weights = c("wls", "ols", "mint", "nseries"),
		fmethod = c("ets", "arima", "rw"),
		algorithms = c("lu", "cg", "chol", "recursive", "slm"),
		covariance = c("shr", "sam"),
		keep.fitted = FALSE, keep.resid = FALSE,
		keep.model = FALSE, keep.intervals = FALSE,
		do.season = FALSE, allow.negative = TRUE, allow.reduced = FALSE,
		positive = FALSE, lambda = NULL, level,
		parallel = FALSE, num.cores = 2, FUN = NULL,
		xreg = NULL, newxreg = NULL, ...) {
	# Forecast hts or gts objects
	#
	# Args:
	#	 object*: Only hts/gts can be passed onto this function.
	#	 h: h-step forecasts.
	#	 method: Aggregated approaches.
	#	 fmethod: Forecast methods.
	#	 keep: Users specify what they'd like to keep at the bottom level.
	#	 do.season: Allow use of lower level seasonal models even if top level is not seasonal
	#	 positive & lambda: Use Box-Cox transformation.
	#	 level: Specify level for the middle-out approach, starting with level 0.
	#
	# Return:
	#	 Point forecasts with other info chosen by the user.
	method <- match.arg(method)

	# Recode old weights arguments
	if(length(weights)==1L) {
		if(weights=="sd") weights <- "wls" else if(weights=="none") weights <- "ols"
	}
	weights <- match.arg(weights)
	covariance <- match.arg(covariance)
	alg <- match.arg(algorithms)
	if (is.null(FUN)) {
		fmethod <- match.arg(fmethod)
	}

	# Error Handling:
	if (!is.gts(object)) {
		stop("Argument object must be either a hts or gts object.", call. = FALSE)
	}
	is_hts <- if (is.hts(object)) TRUE else FALSE
	if (h < 1L) {
		stop("Argument h must be positive.", call. = FALSE)
	}
	if (!is_hts && is.element(method, c("mo", "tdgsf", "tdgsa", "tdfp"))) {
		stop("Argument method is not appropriate for a non-hierarchical time series.", call. = FALSE)
	}
	if (method == "mo" && missing(level)) {
		stop("Please specify argument level for the middle-out method.", call. = FALSE)
	}

	# Set up lambda for arg "positive" when lambda is missing
	if (is.null(lambda)) {
		if (positive) {
			if (any(object$bts <= 0L, na.rm=FALSE)) {
				stop("All data must be positive.", call. = FALSE)
			} else {
				lambda <- 0
			}
		} else {
			lambda <- NULL
		}
	}

	# Remember the original keep.fitted argument for later
	keep.fitted0 <- keep.fitted
	if (method == "comb" && (weights == "mint" || weights == "wls")) {
		keep.fitted <- TRUE
	}

	multilevel <- if (any(method == c("comb", "tdfp"))) TRUE else FALSE
	# Set up "level" for middle-out
	if (method == "mo") {
		len <- length(object$nodes)
		if (level < 0L || level > len) {
			stop("Argument level is out of the range.", call. = FALSE)
		} else if (level == 0L) {
			method <- "tdfp"
		} else if (level == len) {
			method <- "bu"
		} else {
			mo.nodes <- object$nodes[level:len]
			level <- seq(level, len)
			multilevel <- if (length(level) > 1) TRUE else FALSE
		}
	}

	# Set up forecast methods
	if (any(method == c("comb", "tdfp"))) { # Combination or tdfp
		yagg <- aggts(object)	# Grab all ts
	} else if (method == "bu") {	# Bottom-up approach
		yagg <- object$bts	# Only grab the bts
	} else if (any(method == c("tdgsa", "tdgsf")) && method != "tdfp") {
		yagg <- aggts(object, levels = 0)	# Grab the top ts
	} else if (method == "mo") {
		yagg <- aggts(object, levels = level)
	}
	ynames <- colnames(yagg)
	print(paste("forecast.gts: using method", method, "; number of object columns:", length(ynames)))
	# reduce the work done in hts forecasts by only doing unique time-series
	if (is_hts && multilevel && allow.reduced) {
		nd <- if (method == "mo") mo.nodes else object$nodes
		red <- redparams(nd)
		y <- yagg[,red$uniq]
	} else {
		y <- yagg
	}
	level0.seas <- TRUE

# Different version of loopfn which can determine when we are doing level0
# which enables special treatment of seasonality at lower levels
seasfn <- function(xall, n, i, ...) {
	out <- list()
	x <- xall[,i]
	# print(paste("forecast.gts: seasfn: level:", n[[i]]))
	level0 <- if (n[[i]] == "Total") TRUE else FALSE
	if (is.null(FUN)) {
		if (fmethod == "ets") {
			# NB level0.seas initialised to TRUE above
			# Disallow seasonal models if level 0 is not seasonal,
			# this may override user specified parameters
			modelspec <- if (level0.seas) "ZZZ" else "ZZN"
			# print(paste("seasfn: using ets: model specification", modelspec))
			models <- ets(x, model=modelspec, lambda=lambda, ...)
			if (level0) {
			# note use of <<- assignment, i.e. to define variable in global environment
				level0.seas <<- isSeasonal(models, "ets")
				print(paste("seasfn: ets: is seasonal at top level returned", level0.seas))
			}
			fc <- if (keep.intervals) forecast(models, h=h) else forecast(models, h=h, PI=FALSE)
			} else if (fmethod == "arima") {
				if (level0) {
				# default for auto.arima is seasonal=TRUE
				models <- auto.arima(x, lambda=lambda, xreg=xreg, parallel=FALSE, ...)
				level0.seas <<- isSeasonal(models, "arima")
				print(paste("seasfn: arima: is seasonal at top level returned", level0.seas))
			} else {
				models <- auto.arima(x, seasonal=level0.seas, lambda=lambda, xreg=xreg, parallel=FALSE, ...)
			}
			fc <- forecast(models, h=h, xreg=newxreg)
		} else if (fmethod == "rw") {
			# random walk method is arima(0,1,0) and doing it this way guarantees we get the same structure out
			# check we have some non-zero values
			# Matrix is imported
			mdrift <- if(Matrix::nnzero(x) != 0) TRUE else FALSE
			if (level0) {
				models <- Arima(x, order=c(0,1,0), xreg=xreg, include.drift=mdrift, lambda=lambda, ...)
				level0.seas <<- isSeasonal(models, "rw")
			} else {
				mseas <- if (level0.seas) list(order=c(0,1,0),period=12) else list(order=c(0,0,0))
				models <- Arima(x, order=c(0,1,0), seasonal=mseas, include.drift=mdrift, lambda=lambda, xreg=xreg, ...)
			}
			fc <- forecast(models, h=h, xreg=newxreg)
		}
	} else { # user defined function to produce point forecasts
		models <- FUN(x, ...)
		fc <- forecast(models, h = h)
	}

	out$pfcasts <- fc$mean
	if (identical(allow.negative,FALSE)) { out$pfcasts[out$pfcasts<0] <- 0 }

	if (keep.fitted) out$fitted <- stats::fitted(models)
	if (keep.resid) out$resid <- stats::residuals(models)
	if (keep.model) out$model <- stdModel(models,fmethod)
	if (keep.intervals) {
		out$upper <- fc$upper
		out$lower <- fc$lower
	}
	return(out)
}

# loop function which allows seasonal at all levels to grab pf, fitted, resid
loopfn <- function(x, ...) {
	out <- list()
	if (is.null(FUN)) {
		if (fmethod == "ets") {
			models <- ets(x, lambda = lambda, ...)
			fc <- if (keep.intervals) forecast(models, h=h) else forecast(models, h=h, PI=FALSE)
		} else if (fmethod == "arima") {
	# defalult is seasonal=TRUE
			models <- auto.arima(x, lambda=lambda, xreg=xreg, parallel=FALSE, ...)
			fc <- forecast(models, h=h, xreg=newxreg)
		} else if (fmethod == "rw") {
			# Matrix is imported
			mdrift <- if(Matrix::nnzero(x) != 0) TRUE else FALSE
			# random walk method is arima(0,1,0) and doing it this way guarantees we get the same structure out
			# default is seasonal=c(0,0,0)
			models <- Arima(x, order=c(0,1,0), seasonal=list(order=c(0,1,0),period=12), xreg=xreg, include.drift=mdrift, lambda=lambda, ...)
			fc <- forecast(models, h=h, xreg=newxreg)
		}
	} else { # user defined function to produce point forecasts
		models <- FUN(x, ...)
		if (is.null(newxreg)) {
			fc <- forecast(models, h=h)
		} else {
			fc <- forecast(models, h=h, xreg=newxreg)
		}
	}
	out$pfcasts <- fc$mean
	if (identical(allow.negative,FALSE)) { out$pfcasts[out$pfcasts<0] <- 0 }

	if (keep.fitted) out$fitted <- stats::fitted(models)
	if (keep.resid) out$resid <- stats::residuals(models)
	if (keep.model) out$model <- stdModel(models,fmethod)
	if (keep.intervals) {
		out$upper <- fc$upper
		out$lower <- fc$lower
	}
	return(out)
}

	if (parallel) { # parallel == TRUE
		if (is.null(num.cores)) num.cores <- detectCores()
		# Parallel start new process
		lambda <- lambda
		xreg <- xreg
		newxreg <- newxreg
		cl <- makeCluster(num.cores)
		if (do.season) {
			# do seasonal at any level
			# Probably should just use parLapplyLB as the simplify=FALSE arg essentially implies that parSapplyLB is the same
			# loopout <- parSapplyLB(cl=cl, X=y, FUN=function(x) loopfn(x, ...), simplify=FALSE)
			lout <- parLapplyLB(cl=cl, X=y, FUN=function(x) loopfn(x, ...))
		} else {
			# only do seasonal at other levels if top level is seasonal
			# https://stackoverflow.com/questions/47346810/parsapply-with-2-arguments
			# loopout <- parSapplyLB(cl=cl,X=seq(to=ncol(y)),FUN=seasfn,xall=y,n=colnames(y), simplify=FALSE)
			lout <- parLapplyLB(cl=cl,X=seq(to=ncol(y)),FUN=seasfn,xall=y,n=colnames(y))
		}
		stopCluster(cl = cl)
	} else {	# parallel = FALSE
		if (do.season) {
			# do seasonal at any level
			lout <- lapply(y, function(x) loopfn(x, ...))
		} else {
			# only do seasonal at other levels if top level is seasonal
			lout <- lapply(seq(to=ncol(y)), seasfn, xall=y, n=colnames(y))
		}
	}
	if (is_hts && multilevel && allow.reduced && !all(red$uniq)) {
		print(paste("forecast.gts: rebuilding original matrix with forecasts, red type is ", typeof(red)))
		loopout <- lapply(seq(to=ncol(yagg)),FUN=rebuild,y=lout,u=red$uniq,m=red$map)
		# reassign original to y as well
		y <- yagg
	} else {
		loopout <- lout
	}
	# the top level names seem to be missing, so re-apply them
	names(loopout) <- ynames

pifun <- function(x) {
	nr2 <- nrow(x)
	nr <- nr2 %/% 2
	nr1 <- nr+1
	x1 <- x[1:nr,]
	x2 <- x[nr1:nr2,]
	out <- cbind(x1,x2)
	return(out)
}

	# Has all levels here, because lapply() has run for each element and level
	# sapply returns a list after applying the fuction to each element of loopout
	# nb keep.model is picked up below as it does not depend on the method
	pfcasts <- sapply(loopout, function(x) x$pfcasts)
	str(pfcasts)
	if (keep.fitted) fits <- sapply(loopout, function(x) x$fitted)
	if (keep.resid) resid <- sapply(loopout, function(x) x$resid)
	if (keep.intervals) {
		upper <- sapply(loopout, function(x) x$upper)
		lower <- sapply(loopout, function(x) x$lower)
	}

	if (is.vector(pfcasts)) {	# if h = 1, sapply returns a vector
		pfcasts <- t(pfcasts)
	}

	# Set up basic info
	tsp.y <- tsp(y)
	bnames <- colnames(object$bts)

	if (method == "comb") { # Assign class
		class(pfcasts) <- class(object)
		if (keep.fitted) class(fits) <- class(object)
		if (keep.resid) class(resid) <- class(object)
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
		} else if (weights == "wls") {
			tmp.resid <- y - fits # it ensures resids are additive errors
			wvec <- 1/sqrt(colMeans(tmp.resid^2, na.rm = TRUE))
		} else if (weights == "mint") {
			tmp.resid <- stats::na.omit(y - fits)
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

# An internal function to call MinT correctly
mint <- function(x, ...) {
	if (is.hts(x)) {
		return(MinT(x, nodes = object$nodes, ... ))
	} else {
		return(MinT(x, groups = object$groups, ...))
	}
}

	# We only use bts here and rely on predict.R to get the aggts (using aggtts())
	# Method "comb"
	if (method == "comb") {
		if (weights == "ols") {
			bfcasts <- Comb(pfcasts, keep = "bottom", algorithms = alg)
		} else if (any(weights == c("wls", "nseries"))) {
			bfcasts <- Comb(pfcasts, weights = wvec, keep = "bottom", algorithms = alg)
		} else { # weights = "mint"
			bfcasts <- mint(pfcasts, residual = tmp.resid, covariance = covariance, keep = "bottom", algorithms = alg)
	}

		if (keep.fitted0) {
			if (weights == "ols") {
				fits <- Comb(fits, keep = "bottom", algorithms = alg)
			} else if (any(weights == c("wls", "nseries"))) {
				fits <- Comb(fits, weights = wvec, keep = "bottom", algorithms = alg)
			} else { # weights = "mint"
				 fits <- mint(fits, residual = tmp.resid, covariance = covariance, keep = "bottom", algorithms = alg)
			}
		}
		if (keep.resid) {
			if (weights == "ols") {
				resid <- Comb(resid, keep = "bottom", algorithms = alg)
			} else if (any(weights == c("wls", "nseries"))) {
				resid <- Comb(resid, weights = wvec, keep = "bottom", algorithms = alg)
			} else { # weights = "mint"
				resid <- mint(resid, residual = tmp.resid, covariance = covariance, keep = "bottom", algorithms = alg)
			}
		}
		if (keep.intervals) {
			if (weights == "ols") {
				upper <- Comb(upper, keep = "bottom", algorithms = alg)
				lower <- Comb(lower, keep = "bottom", algorithms = alg)
			} else if (any(weights == c("wls", "nseries"))) {
				upper <- Comb(upper, weights = wvec, keep = "bottom", algorithms = alg)
				lower <- Comb(lower, weights = wvec, keep = "bottom", algorithms = alg)
			} else { # weights = "mint"
				 upper <- mint(upper, residual = tmp.resid, covariance = covariance, keep = "bottom", algorithms = alg)
				 lower <- mint(lower, residual = tmp.resid, covariance = covariance, keep = "bottom", algorithms = alg)
			}
		}

	# Method "bu"
	} else if (method == "bu") {
		bfcasts <- pfcasts

	# Method "tdgsa"
	} else if (method == "tdgsa") {
		bfcasts <- TdGsA(pfcasts, object$bts, y)
		if (keep.fitted0) fits <- TdGsA(fits, object$bts, y)
		if (keep.resid) resid <- TdGsA(resid, object$bts, y)
		if (keep.intervals) {
			upper <- TdGsA(upper, object$bts, y)
			lower <- TdGsA(lower, object$bts, y)
		}

	# Method "tdgsf"
	} else if (method == "tdgsf") {
		bfcasts <- TdGsF(pfcasts, object$bts, y)
		if (keep.fitted0) fits <- TdGsF(fits, object$bts, y)
		if (keep.resid) resid <- TdGsF(resid, object$bts, y)
		if (keep.intervals) {
			upper <- TdGsF(upper, object$bts, y)
			lower <- TdGsF(lower, object$bts, y)
		}

	# Method "tdfp"
	} else if (method == "tdfp") {
		bfcasts <- TdFp(pfcasts, object$nodes)
		if (keep.fitted0) fits <- TdFp(fits, object$nodes)
		if (keep.resid) resid <- TdFp(resid, object$nodes)
		if (keep.intervals) {
			upper <- TdFp(upper, object$nodes)
			lower <- TdFp(lower, object$nodes)
		}

	# Method "mo"
	# Need to fix this
	} else if (method == "mo") {
		bfcasts <- MiddleOut(pfcasts, mo.nodes)
		if (keep.fitted0) fits <- MiddleOut(fits, mo.nodes)
		if (keep.resid) resid <- MiddleOut(resid, mo.nodes)
		if (keep.intervals) {
			upper <- MiddleOut(upper, mo.nodes)
			lower <- MiddleOut(lower, mo.nodes)
		}
	}

	# In case that accuracy.gts() is called later, since NA's have been omitted
	# to ensure slm/chol to run without errors.
	if (method == "comb" && fmethod == "rw" && keep.fitted0 == TRUE && (alg == "slm" || alg == "chol")) {
		fits <- rbind(rep(NA, ncol(fits)), fits)
	}

	# Convert back to time-series
	# bfcasts will have different sets data depending on the method above, e.g.
	# tdfp - only bts
	# comb - all levels
	fcasts <- ts(bfcasts, start = tsp.y[2L] + 1L/tsp.y[3L], frequency = tsp.y[3L])
	fcols <- ncol(fcasts)
	if (fcols == length(bnames)) {
		onames = bnames
	} else if (fcols == length(ynames)) {
		onames = ynames
	} else {
		print(paste("forecast.gts: bottom level forecasts number of columns:", fcols))
	}
	colnames(fcasts) <- onames
	class(fcasts) <- class(object$bts)
	attr(fcasts, "msts") <- attr(object$bts, "msts")

	if (keep.fitted0) {
		fits <- ts(fits, start = tsp.y[1L], frequency = tsp.y[3L])
		# print(paste("forecast.gts: fitted columns:", ncol(fits)))
		colnames(fits) <- onames
	}
	if (keep.resid) {
		resid <- ts(resid, start = tsp.y[1L], frequency = tsp.y[3L])
		# print(paste("forecast.gts: resid columns:", ncol(resid)))
		colnames(resid) <- onames
	}
	if (keep.intervals) {
		upper <- pifun(upper)
		upper <- ts(upper, start = tsp.y[2L] + 1L/tsp.y[3L], frequency = tsp.y[3L])
		ucols <- ncol(upper)
		# print(paste("forecast.gts: upper predictio interval number of columns:", ucols))
		pinames <- c(paste(onames,".80",sep=""), paste(onames,".95",sep=""))
		colnames(upper) <- pinames
		lower <- pifun(lower)
		lower <- ts(lower, start = tsp.y[2L] + 1L/tsp.y[3L], frequency = tsp.y[3L])
		colnames(lower) <- pinames
	}

	# local function used below
	copyModel <- function(y, i, ...) {
		# assign the model for each element in out list from loopout list
		# (the out variable in both seasfn and loopfn)
		cm <- y[[i]]$model
		return(cm)
	}

	# Output
	# output a variety of levels for fcasts (depending on method) and bottom level for historic
	# for fits, resid and intervals we recreate the higher levels outside of this function using aggts()
	out <- list(bts = fcasts, histy = object$bts, labels = object$labels, method = method, fmethod = fmethod)

	if (keep.fitted0) out$fitted <- fits
	if (keep.resid) out$residuals <- resid
	if (keep.intervals) {
		out$upper <- upper
		out$lower <- lower
	}
	if (keep.model) {
		model <- list()
		model <- lapply(seq(to=ncol(y)), copyModel, y=loopout)
		# we have all models for all levels here
		names(model) <- ynames
		out$model <- model
	}

	if (is_hts) {
		out$nodes <- object$nodes
	} else {
		out$groups <- object$groups
	}

	return(structure(out, class = class(object)))
}
