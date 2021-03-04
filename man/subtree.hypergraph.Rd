\name{subtree.hypergraph}
\alias{subtree.hypergraph}
\title{
Subtree Hypergraph.
}
\description{
Construct a subtree hypergraph from a graph.
}
\usage{
subtree.hypergraph(g, v)
}
\arguments{
  \item{g}{
a graph.
}
  \item{v}{
a list of vertex sets. See details.
}
}
\details{
A subtree hypergraph is a hypergraph on the vertices of the graph \code{g},
each of whose hyper-edges induces a subtree in \code{g}. If \code{v}
is given, each
element of the list must contain at least two elements. For each element of
\code{v}, all paths between it's first element and each of the other elements 
are computed, and the hyper-edge corresponds to all the vertices in these
paths.
}
\value{
a hypergraph.
}
\references{
  Bretto, Alain, Hypergraph theory, An introduction. Springer, 2013.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\note{
There are many possible subtree hypergraphs for a given graph. The 
default is to loop through the vertices in a particular way to generate
a hypergraph. The intent is that the use should provide \code{v}, rather
than using this admittedly arbitrary algorithm.
}

\examples{
   g <- make_tree(20,mode='undirected')
}
\keyword{ hypergraphs }
