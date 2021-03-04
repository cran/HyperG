\name{hypergraph.union}
\alias{hypergraph.union}
\alias{hypergraph.disjoint.union}
\alias{hypergraph.intersection}
\title{
Unions and intersections of hypergraphs.
}
\description{
Given two hypergraphs, compute their union or intersection.
}
\usage{
hypergraph.union(h1, h2, reduce = TRUE)
hypergraph.disjoint.union(h1, h2)
hypergraph.intersection(h1, h2, strict = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h1,h2}{
hypergraphs.
}
  \item{reduce}{
logical. Whether to reduce the resultant hypergraph by removing edges
that are subsets of other edges.
}
  \item{strict}{
logical. See details.
}
}
\details{
The disjoint union of two hypergraphs is a hypergraph on the disjoint union
of the vertices. The vertices are renamed,
if necessary, so that those in the first hypergraph
are distinct from those in the second.
All edges that occur in either hypergraph,
with the vertices renamed, are retained. The (non-disjoint) union treats
vertices with the same name (or if they are unnamed, the same index) as the
same vertex, and produces the hypergraph containing all edges that are in
either hypergraph. If \code{reduce} is TRUE, the hypergraph is reduced so
that hyper-edges that are subsets of another edge are removed. For the 
intersection, only those edges that are in one of the hypergraphs are
retained. Again, vertices with the same name are consider to be the same, and
only these vertices are retained. If \code{strict} is true, the edges must be
exactly the same. Otherwise, an edge in one hypergraph that is a subset of
an edge in the other will be retained.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
   h1 <- hypergraph_from_edgelist(list(1:3,3:5,4:9,2:6))
   h2 <- hypergraph_from_edgelist(list(2:3,3:5,4:9,2:6,c(3,5,10:11)))
	hypergraph.disjoint.union(h1,h2)
	hypergraph.union(h1,h2)
	hypergraph.intersection(h1,h2)
}
\keyword{ hypergraphs }
