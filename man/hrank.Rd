\name{hrank}
\alias{hrank}
\alias{hcorank}
\title{
Rank of a hypergraph.
}
\description{
Return the rank and corank of a hypergraph. The rank is the maximum
cardinality of a hyperedge, the corank (sometimes called the anti-rank)
is the minimum.
}
\usage{
hrank(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\value{
a number.
}
\references{
  Bretto, Alain, Hypergraph theory, An introduction. Springer, 2013.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
h <- hypergraph_from_edgelist(list(1:5,4:7,c(1,6)))
hrank(h)   # 5
hcorank(h) # 2
}
\keyword{ hypergraphs }
