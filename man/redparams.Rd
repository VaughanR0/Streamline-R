\name{redparams}
\alias{redparams}
\title{Create supporting vectors for hts object so forecasts are only run on unique columns}
\description{
A helper function for creating the parameters required to reduce the forecast set
and then rebuild the full set post forecasts
}
\usage{
redparams(y,nodes)
}

\arguments{
  \item{y}{the full set of time-series columns for forecasts}
  \item{nodes}{the nodes component of the hts structure}
}
\author{Vaughan Roberts}

\seealso{\code{\link[hts]{allts}}}

\examples{
	\dontrun{
		yagg <- aggts(htseg3)
		r <- redparams(yagg)
	}
}
\keyword{ts}
