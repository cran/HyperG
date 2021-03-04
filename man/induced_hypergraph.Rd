\name{induced_hypergraph}
\alias{induced_hypergraph}
\title{
Induced hypergraph.
}
\description{
Computes the hypergraph induced by a subset of the vertices.
}
\usage{
induced_hypergraph(h, v, simplify = TRUE)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{v}{
a vector of vertices.
}
  \item{simplify}{
logical.
}
}
\details{
First the hypergraph is reduced to only those vertices in \code{v}. This
results in it retaining only those hyper-edges containing any elements of 
\code{v}, as well as removing from the resultant hyper-edges any vertices
not in \code{v}. If \code{simplify} is true, loops are then removed. This
function always removes empty hyper-edges, so any hyper-edge which does
not contain any elements of \code{v} is removed.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
   h <- hypergraph_from_edgelist(list(1:4,3:7,c(1,3,5)))
	k <- induced_hypergraph(h,c(1,3,5))
}
\keyword{ hypergraphs }
