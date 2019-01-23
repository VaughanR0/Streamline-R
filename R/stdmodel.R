stdModel <- function(model, type = c("ets", "arima", "rw"), ...) {
	# returns model parameters in a consistent list structure
	out <- list()
	mdebug <- FALSE
	# print("stdModel: model")
	# print(model)
	# print("stdModel: structure")
	# print(str(model))

	out$type <- type
	if (type == "ets") {
		par <- list()
		out$loglik <- signif(model$loglik,4)
		out$aic <- signif(model$aic,4)
		out$aicc <- signif(model$aicc,4)
		out$bic <- signif(model$bic,4)
		out$sigma2 <- signif(model$sigma2,4)
		if (mdebug) print(paste("StdModel: ETS: loglik", out$loglik, "aic", out$aic, "aicc", out$aicc, "bic", out$bic, "sigma", out$sigma2))
		par$E <- model$components[1]
		par$T <- model$components[2]
		par$S <- model$components[3]
		out$par <- par
		return(out)
	}

	if (any(type == c("arima", "rw"))) {
		# "rw" is now specified as an (0,1,0) arima model using Arima() function
		par <- list()
		out$loglik <- signif(model$loglik,4)
		out$aic <- signif(model$aic,4)
		out$aicc <- signif(model$aicc,4)
		out$bic <- signif(model$bic,4)
		out$sigma2 <- signif(model$sigma2,4)
		if (mdebug) print(paste("StdModel: arima: loglik", out$loglik, "aic", out$aic, "aicc", out$aicc, "bic", out$bic, "sigma", out$sigma2))
		par$p <- model$arma[1]
		par$q <- model$arma[2]
		par$d <- model$arma[6]
		par$m <- model$arma[5]
		par$P <- model$arma[3]
		par$Q <- model$arma[4]
		par$D <- model$arma[7]
		# Do drift or non-zero-mean as well
		par$drift <- signif(model$coef[grepl('drift',names(model$coef))],4)
		par$intercept <- signif(model$coef[grepl('intercept',names(model$coef))],4)
		if (mdebug) print(paste("StdModel: drift", par$drift, "intercept", par$intercept))
		out$par <- par
		return(out)
	}
}

isSeasonal <- function(model, type = c("ets", "arima", "rw"), ...) {
	# returns logical value indicating whether the fitted model is seasonal
	flg = FALSE
	if (type == "ets") {
		flg <- grepl("[MA])", model$method)
	}
	if (any(type == c("arima", "rw"))) {
		# $arma gives a vector with (p, q, P, Q, m, d, D) where m is seasonal frequency
		flg <- ifelse(model$arma[5] > 0, TRUE, FALSE)
	}
	return (flg)
}

#' Returns the aggregates at all levels for a hts object
#'
#' @rdname helper-functions
#' @param y A time-series object, e.g. h$bts, where h is a hts object
#' @param nodes The node description of the hts object, a list
#' @param labs The labels of the nodes
#' @return a multi-variate time-series
#' @author Vaughan Roberts
#' @export
aggtts <- function(y, nodes, labs) {
	# returns the aggregates at all levels for a time-series object
	if (!is.ts(y)) stop("'y' must be a time-series object", call. = FALSE)
	gmat <- GmatrixH(nodes)
	levels <- 1L:nrow(gmat)

	# a function to aggregate the time-series, (sums row of transpose(y))
	rSum <- function(y, i) rowsum(t(y), gmat[i,], reorder=FALSE, na.rm=TRUE)
	ally <- lapply(levels, rSum, y=y)
	# Convert to matrix while applying a transposition
	ally <- matrix(unlist(sapply(ally,t)), nrow = nrow(y))

	colnames(ally) <- unlist(labs[levels])
	y.tsp <- stats::tsp(y)
	ally <- ts(ally, start=y.tsp[1L], frequency=y.tsp[3L])
	return(ally)
}
