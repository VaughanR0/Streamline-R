\name{forecast.gts}
\Rdversion{1.1}
\alias{forecast.gts}
\alias{forecast.hts}
\title{Forecast a hierarchical or grouped time series}
\description{
Methods for forecasting hierarchical or grouped time series.
}
\usage{
\S3method{forecast}{gts}(object,
		h = ifelse(frequency(object$bts) > 1L, 2L * frequency(object$bts), 10L), 
		method = c("comb", "bu", "mo", "tdgsa", "tdgsf", "tdfp"),
		weights = c("wls", "ols", "mint", "nseries"),
		fmethod = c("ets", "arima", "rw"), 
		algorithms = c("lu", "cg", "chol", "recursive", "slm"),
		covariance = c("shr", "sam"),
		keep.fitted = FALSE, keep.resid = FALSE,
		keep.model = FALSE, keep.intervals = FALSE,
		do.season = FALSE, allow.negative = FALSE,
		positive = FALSE, lambda = NULL, level, 
		parallel = FALSE, num.cores = 2, FUN = NULL,
		xreg = NULL, newxreg = NULL, ...)
}
\arguments{
  \item{object}{Hierarchical or grouped time series object of class \code{{gts}}}
  \item{h}{Forecast horizon}
  \item{method}{Method for distributing forecasts within the hierarchy. See details}
  \item{weights}{Weights used for "optimal combination" method:
		\code{weights="ols"} uses an unweighted combination (as described in Hyndman et al 2011);
		\code{weights="wls"} uses weights based on forecast variances (as described in Hyndman et al 2015);
		\code{weights="mint"} uses a full covariance estimate to determine the weights (as described in Hyndman et al 2016);
		\code{weights="nseries"} uses weights based on the number of series aggregated at each node.}
  \item{fmethod}{Forecasting method to use}
  \item{algorithms}{An algorithm to be used for computing the combination forecasts (i.e. when \code{method=="comb"}).
	The combination forecasts are based on an ill-conditioned regression model.
	"lu" indicates LU decomposition is used;
	"cg" indicates a conjugate gradient method;
	"chol" corresponds to a Cholesky decomposition;
	"recursive" indicates the recursive hierarchical algorithm of Hyndman et al (2015);
	"slm" uses sparse linear regression.
	Note that \code{algorithms = "recursive"} and \code{algorithms = "slm"} cannot be used if \code{weights="mint"}.}
  \item{covariance}{Type of the covariance matrix to be used with \code{weights="mint"}:
	either a shrinkage estimator (\code{"shr"}) with shrinkage towards the diagonal;
	or a sample covariance matrix (\code{"sam"}).}
  \item{keep.fitted}{If TRUE, keep fitted values at all levels.}
  \item{keep.resid}{If TRUE, keep residuals at all levels.}
  \item{keep.model}{If TRUE, keep model descriptions at all levels.}
  \item{keep.intervals}{If TRUE, keep prediction intervals at all levels.}
  \item{do.season}{If TRUE, allow seasonal models at the lower levels if even the top level is not seasonal.}
  \item{allow.negative}{If TRUE, forecasts are not truncated to 0 when negative use this as setting \code{lambda=0} requires all bts to be strictly positive and in my experience this is not reliable anyway - probably due to re-allocation between levels.}
  \item{positive}{If TRUE, forecasts are forced to be strictly positive}
  \item{lambda}{Box-Cox transformation parameter}
  \item{level}{Level used for "middle-out" method (only used when \code{method="mo"})}
  \item{parallel}{If TRUE, import \code{parallel} package to allow parallel processing}
  \item{num.cores}{If parallel = TRUE, specify how many cores are going to be used}
  \item{FUN}{A user-defined function that returns an object which can be passed to the 
  \code{forecast} function. It is applied to all series in order to generate base forecasts. 
  When \code{FUN} is not \code{NULL}, \code{fmethod}, \code{positive} and \code{lambda} are
  all ignored. Suitable values for \code{FUN} are \code{\link[forecast]{tbats}} and 
  \code{\link[forecast]{stlf}}.}
  \item{xreg}{When \code{fmethod = "arima"}, a vector or matrix of external regressors used for modelling, which must have the same number of rows as the original univariate time series}
  \item{newxreg}{When \code{fmethod = "arima"}, a vector or matrix of external regressors used for forecasting, which must have the same number of rows as the \code{h} forecast horizon}
  \item{...}{Other arguments passed to \code{\link[forecast]{ets}}, 
  \code{\link[forecast]{auto.arima}} or \code{FUN}.}
}
\value{A forecasted hierarchical/grouped time series of class \code{gts}.}
\details{
Base methods implemented include ETS, ARIMA and the naive (random walk) models. 
Forecasts are distributed in the hierarchy using bottom-up, top-down, middle-out and
optimal combination methods. 

Three top-down methods are available: the two Gross-Sohl
methods and the forecast-proportion approach of Hyndman, Ahmed, and Athanasopoulos (2011).
The "middle-out" method \code{"mo"} uses bottom-up (\code{"bu"}) for levels higher than 
\code{level} and top-down forecast proportions (\code{"tdfp"}) for levels lower than \code{level}. 

For non-hierarchical grouped data, only bottom-up and combination methods are possible, as any method involving top-down disaggregation requires a hierarchical ordering of groups.

When \code{xreg} and \code{newxreg} are passed, the same covariates are applied to every series in the hierarchy.
}
\references{
G. Athanasopoulos, R. A. Ahmed and R. J. Hyndman (2009) Hierarchical forecasts for Australian domestic tourism,
\emph{International Journal of Forecasting}, \bold{25}, 146-166.

R. J. Hyndman, R. A. Ahmed, G. Athanasopoulos and H.L. Shang (2011)
Optimal combination forecasts for hierarchical time series.
\emph{Computational Statistics and Data Analysis}, \bold{55}(9), 2579--2589.
\url{http://robjhyndman.com/papers/hierarchical/}

Hyndman, R. J., Lee, A., & Wang, E. (2014). 
Fast computation of reconciled forecasts for hierarchical and grouped time series. 
\emph{Working paper 17/14, Department of Econometrics & Business Statistics, Monash University.}
\url{http://robjhyndman.com/working-papers/hgts/}

Gross, C. and Sohl, J. (1990) Dissagregation methods to expedite product line forecasting,
\emph{Journal of Forecasting}, \bold{9}, 233-254.
}

\author{Earo Wang and Rob J Hyndman}

\seealso{
 \code{\link[hts]{hts}}, \code{\link[hts]{gts}}, \code{\link[hts]{plot.gts}}, \code{\link[hts]{accuracy.gts}}
}
\examples{
forecast(htseg1, h = 10, method = "bu", fmethod = "arima")

\dontrun{forecast(htseg2, h = 10, method = "comb", algorithms = "lu",
                  FUN = function(x) tbats(x, use.parallel = FALSE))}
}
\keyword{ts}

