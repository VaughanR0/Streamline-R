\name{aggtts}
\alias{aggtts}
\title{Aggregate all levels of a hts object}
\description{
The time series from selected levels of a hierarchical/grouped time series or a forecasted
hierarchical/grouped time series are returned as a multivariate time series.
}
\usage{
aggtts(y, nodes, labs)
}

\arguments{
  \item{y}{An object of class \code{{hts}}.}
  \item{nodes}{A list describing the node structure in the hts object.}
  \item{labs}{A list of labels of the nodes.}
}
\author{Vaughan Roberts}

\seealso{\code{\link[hts]{allts}}}

\examples{
\dontrun{aggtts(htseg2$bts, htseg2$nodes, htseg2$labels)}
aggtts(htseg2$bts, nodes=list(2,c(2,2),c(3,2,3,2)), labs=colnames(htseg2))
}
\keyword{ts}
