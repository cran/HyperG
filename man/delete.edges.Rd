\name{hypergraph.delete.edges}
\alias{hypergraph.delete.edges}
\alias{delete.hyperedges}
\alias{hypergraph.delete.vertices}
\title{
Delete edges or vertices of a hypergraph.
}
\description{
Remove edges or vertices from a hypergraph.
}
\usage{
hypergraph.delete.edges(h, edges)
delete.hyperedges(h, edges)
hypergraph.delete.vertices(h, v)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{edges,v}{
A vector of edges or vertices (indices) to remove.
}
}
\details{
\code{delete.hyperedges} is an alias for \code{hypergraph.delete.edges}.
}
\value{
a hypergraph
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{delete.vertices}},
\code{\link[igraph]{delete.edges}}.
}

\examples{
   h <- hypergraph_from_edgelist(list(1:3,3:7,7:8))
	hypergraph.delete.vertices(h,3)
	hypergraph.delete.vertices(h,7)
	hypergraph.delete.edges(h,2)
}
\keyword{ hypergraphs }
