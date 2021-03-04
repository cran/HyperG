\name{hypergraph.add.edges}
\alias{hypergraph.add.edges}
\alias{add.hyperedges}
\alias{hypergraph.add.vertices}
\title{
Add edges or vertices to a hypergraph.
}
\description{
Adds hyper-edges or vertices to a hypergraph.
}
\usage{
hypergraph.add.edges(h, edges, verbose = FALSE)
add.hyperedges(h, edges, verbose = FALSE)
hypergraph.add.vertices(h, nv, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
A hypergraph. 
}
  \item{edges}{
A list of edges to be added.
}
  \item{nv}{
Number of vertices to add.
}
  \item{names}{
Optional vector of names of the vertices.
}
  \item{verbose}{
logical. Whether to warn if new vertices are created.
}
}
\details{
The edges can be indices or edge names. This is different than the graph
call -- see the \CRANpkg{igraph} package help for that.
If edges is NULL, or missing,
\code{hypergraph.add.edges} adds a single empty hyper-edge to the hypergraph.
\code{add.hyperedges} is an alias for \code{hypergraph.add.edges}.
}
\value{
Returns a hypergraph (or graph) as appropriate.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{add_vertices}},
\code{\link[igraph]{add_edges}}
}
\examples{
   h <- hypergraph_from_edgelist(list(1:4,1:2,c(2,3,5),c(3,5:7)))
	h1 <- hypergraph.add.vertices(h,1,"8")
	h2 <- hypergraph.add.edges(h,list(c(1,5,8),7:9))
}
\keyword{ graphs }
\keyword{ hypergraphs }
\concept{functions for manipulating hypergraph structure}
