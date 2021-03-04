\name{hypergraph.complement}
\alias{hypergraph.complement}
\title{
The complement of a hypergraph.
}
\description{
The complement of a hypergraph is a hypergraph consisting of the hyper-edges
that are not found in the original hypergraph.
}
\usage{
hypergraph.complement(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
The incidence matrix of the complement of \code{h} has a 0 in those places the
original matrix had a 1, and a 1 in those places the original matrix had a 0.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
    h <- hypergraph_from_edgelist(list(c(2,3),c(1,4)))
	 hypergraph.complement(h)
}
\keyword{ hypergraphs }
