\name{print.hypergraph}
\alias{print.hypergraph}
\title{
Print a hypergraph to the console.
}
\description{
Print method for hypergraphs.
}
\usage{
\method{print}{hypergraph}(x, ...)
}
\arguments{
  \item{x}{
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
}
\keyword{ hypergraphs }
