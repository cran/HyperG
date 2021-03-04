\name{summary.hypergraph}
\alias{summary.hypergraph}
\title{
Print a summary of the hypergraph to the console.
}
\description{
Summary method for hypergraphs.
}
\usage{
\method{summary}{hypergraph}(object, ...)
}
\arguments{
  \item{object}{
a hypergraph.
}
  \item{\dots}{
ignored arguments.
}
}
\value{
No return value, called for side effects only -- prints to console.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
   h <- hypergraph_from_edgelist(list(3:7,8:12,c(1,3,9)))
	print(h)
	summary(h)
}
\keyword{ hypergraphs }
