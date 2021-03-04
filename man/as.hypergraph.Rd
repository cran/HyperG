\name{as.hypergraph}
\alias{as.hypergraph}
\alias{as.graph}
\alias{hypergraph2graph}
\alias{graph2hypergraph}
\title{
Convert between hypergraphs and graphs.
}
\description{
Convert a hypergraph to a graph or a graph to a hypergraph.
}
\usage{
as.graph(h)
hypergraph2graph(h)
as.hypergraph(x,n, ...)
graph2hypergraph(g, method = c("incidence", "adjacency", 
                               "neighborhood", "ego",  "spectral"), ...)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{g}{
a graph.
}
  \item{method}{
see Details.
}
  \item{x}{
a matrix, list or graph. See details.
}
  \item{n}{
number of vertices if x is missing.
}
  \item{\dots}{
arguments passed to various functions. See Details.
}
}
\details{
For \code{as.graph} and \code{hypergraph2graph},
create a graph from the incidence matrix using the product of the 
transpose of the incidence matrix with 
the incidence matrix. 
\code{as.graph} is an alias
of \code{hypergraph2graph}. This computes the 2-section of the hypergraph,
in the terminology of Bretto.

The function \code{as.hypergraph()} returns a
hypergraph defined by a graph, matrix (or edgelist). If a matrix is given, it
is viewed as the incidence matrix of the hypergraph. If a list is given, it
is interpreted as the hyper-edge list. If a graph is given, graph2hypergraph
is called with the graph and the arguments passed in ....
If \code{x} is NULL or missing, and \code{n>0} is given, an empty hypergraph
on \code{n} nodes is returned. If all else fails, an empty hypergraph on no
nodes is returned.

The \code{method} variable controls the method used for turning a graph
into a hypergraph:

incidence - use the incidence matrix of the graph.

adjacency - treat the adjacency matrix as the incidence matrix.

neighborhood,ego - Use the neighborhoods of the vertices. The arguments
to \code{ego} are passed in the dotted arguments.

spectral - The spectral embedding is performed, followed by \code{Mclust}.
The arguments are passed to \code{hypergraph_from_spectral_clustering}.

}
\value{
An undirected \CRANpkg{igraph} graph object.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\references{
  Bretto, Alain, Hypergraph theory, An introduction. Springer, 2013.
}

\seealso{
\code{\link{as.hypergraph}}, \code{\link{graph2hypergraph}},
\code{\link{hypergraph_from_spectral_clustering}},
\code{\link[igraph]{ego}},
\code{\link[mclust]{Mclust}},
}
\examples{
   h <- hypergraph_from_edgelist(list(1:4,1:2,c(2,3,5),c(3,5:7)))
	g <- as.graph(h)
}
\keyword{ hypergraphs }
\keyword{ graph }
\concept{functons to convert between graphs and hypergraphs}
