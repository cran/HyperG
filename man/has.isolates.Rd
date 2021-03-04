\name{has.isolates}
\alias{has.isolates}
\alias{has.loops}
\alias{has.empty.hyperedges}
\title{
Test for loops, isolates and empty hyper-edges.
}
\description{
Tools to determine whether a hypergraph has degenerate elements such as loops (hyper-edges with a single vertex) isolated vertices (ones which appear in no hyper-edges) and empty hyper-edges.
}
\usage{
has.isolates(h)
has.loops(h)
has.empty.hyperedges(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\value{
returns a logical.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}
\seealso{
\code{\link{remove.isolates}}, 
\code{\link{remove.loops}}, 
\code{\link{remove.empty.hyperedges}}.
\code{\link{reduce.hypergraph}}.
}
\examples{
   h <- hypergraph_from_edgelist(list(1:3,2:8,9))
	has.loops(h) ## TRUE
	has.isolates(h) ## FALSE
	k <- hypergraph.add.vertices(h,10)
	has.isolates(k) ## TRUE
}
\keyword{ hypergraphs }
