\name{is.simple}
\alias{is.simple}
\alias{is.linear}
\title{
Is a hypergraph simple/linear?
}
\description{
Tests whether a hypergraph is simple or linear.
}
\usage{
is.simple(h)
is.linear(h)
}
\arguments{
  \item{h}{
a hypergraph. Can be a graph for \code{is.simple}, in which case the 
\CRANpkg{igraph} version is called.
}
}
\details{
A hypergraph is simple if all it's edges are distinct, 
non-empty, and if edge i is 
contained in edge j, then i=j. A hypergraph is linear if it is simple and
the intersection of any two hyper-edges has at most one element.

}
\value{
a logical.
}
\references{
Akram, M., & Luqman, A. (2020). Fuzzy hypergraphs and related extensions. 
Springer Singapore.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}
\examples{
h <- hypergraph_from_edgelist(list(1:4,4:7,c(6,8:10),10:14))
is.linear(h)  ## TRUE
is.simple(h)  ## TRUE
}
\keyword{ hypergraphs }
