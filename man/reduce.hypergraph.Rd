\name{reduce.hypergraph}
\alias{reduce.hypergraph}
\alias{simplify.hypergraph}
\alias{remove.loops}
\alias{remove.isolates}
\alias{remove.empty.hyperedges}
\title{
Remove redundant hyperedges and isolated vertices.
}
\description{
Reduce a hypergraph by removing redundant edges, loops, or isolated vertices.
}
\usage{
reduce.hypergraph(h,method="inclusion")
simplify.hypergraph(h)
remove.isolates(h)
remove.loops(h)
remove.empty.hyperedges(h)
remove.loops(h)
}
\arguments{
  \item{h}{
a hypergraph
}
  \item{method}{
character. See details.
}
}
\details{
\code{reduce.hypergraph} removes redundant edges and/or reduces the hyper-edges:
if the method is
"inclusion" this removes hyper-edges that
are contained in other hyper-edges;
if the method is "intersection", it replaces the hypergraph with a new hypergraph
whose edges are intersections of the original hypergraph's hyper-edges -- each pair of hyper-edges e1 and e2 with a non-empty intersection result in a hyper-edge
corresponding to that intersection in the new hypergraph;
if the method is "union" it removes edges that are contained in the union
of the other edges, reducing to a hypegraph in which every hyper-edge contains
at least one vertex which is contained in no other hyper-edge. 

Isolates are vertices that are not in any
hyper-edges, and loops are hyper-edges containing a single vertex.
\code{reduce.hypergraph} will remove edges which contain no vertices, since the
null set is a subset of any hyper-edge and hence is in the union of all other
hyper-edges, so for either method such edges would be removed.
\code{simplify.hypergraph} removes loops, isolates, and empty hyper-edges
from a hypergraph. Isolates are removed after removing loops.
This may reduce the order of the hypergraph, unlike the \CRANpkg{igraph}
\code{\link[igraph]{simplify}} command, which only removes edges. 
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\examples{
   h <- hypergraph_from_edgelist(list(1:4,1:3,2:4,2:6))
	reduce.hypergraph(h)
}
\keyword{ hypergraphs }
