\name{rebuild}
\alias{rebuild}
\title{Rebuild columns of the full results matrix after running forecasts on unique columns}
\description{
A helper function for rebuilding the full set of forecasts for hts returned from aggts() call
after a reduced set of unique data has been forecast.
Usually used as part of an lapply() call
}
\usage{
rebuild(i, y, u, m)
}

\arguments{
  \item{i}{the column index into the full matrix of time-series}
  \item{y}{the reduced matrix of forecasts}
  \item{u}{boolean vector indicating which columns in the full matrix are unique and need to be forecast}
  \item{m}{ numerical vector which maps the removed columns to their equivalents in the full matrix}
}
\author{Vaughan Roberts}

\seealso{\code{\link[hts]{allts}}}

\examples{
	\dontrun{
		yagg <- aggts(htseg3)
		r <- redparams(yagg)
		# remove unnecessary columns prior to forecasts
		y <- yagg[,r$uniq]
		...
		# lout is output from the lapply used to run each forecast
		loopout <- lapply(seq(to=ncol(yagg)),FUN=rebuild, y=lout, u=r$uniq, m=r$map)
	}
}
\keyword{ts}
