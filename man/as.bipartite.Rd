\name{as.bipartite}
\alias{as.bipartite}
\title{
Hypergraph as a bipartite graph.
}
\description{
Converts a hypergraph (or graph) into a bipartite graph.
}
\usage{
as.bipartite(h)
}
\arguments{
  \item{h}{
a hypergraph or a graph.
}
}
\details{
This converts a hypergraph or a graph into a bipartite graph, by
taking the incidence matrix and treating this as the incidence matrix
of a bipartite graph. It uses \code{\link[igraph]{graph_from_incidence_matrix}}
to perform the conversion.
}
\value{
an \CRANpkg{igraph} bipartite graph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}
\note{
This works on graphs, resulting in the bipartite graph with edges as one
type and vertices as another. This might not be what you want, for example
if you think the graph is already bipartite, this will not return the graph,
but will rather create a new bipartite graph from the vertices and edges.
}

\seealso{
\code{\link[igraph]{graph_from_incidence_matrix}}.
}
\examples{
   h <- hypergraph_from_edgelist(list(letters[1:3],
           letters[c(2,4,7)],
  		  	  letters[5:8]))
   g <- as.bipartite(h)


}
\keyword{ graphs }
\keyword{ hypergraphs }
\concept{ bipartite graph }
\concept{ hypergraph representations as graphs }
