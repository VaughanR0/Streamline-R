\name{redparams}
\alias{redparams}
\title{Create supporting vectors from the nodes of a hts object so forecasts are only run on unique columns}
\description{
A helper function for creating the parameters required to eliminate duplication from
the forecast set and then rebuild the full set post forecasts
}
\usage{
redparams(nodes)
}

\arguments{
  \item{nodes}{the nodes component of the hts structure}
}
\author{Vaughan Roberts}

\seealso{\code{\link[hts]{aggts}}}

\examples{
	\dontrun{
		yagg <- aggts(htseg3)
		r <- redparams(htseg3$nodes)
	}
}
\keyword{ts}
