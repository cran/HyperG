\name{hypergraph_as_adjacency_matrix}
\alias{hypergraph_as_adjacency_matrix}
\alias{hadjacency}
\title{
Adjacency Matrix of a Hypergraph.
}
\description{
Returns the adjacency matrix, computed from the incidence matrix.
}
\usage{
hypergraph_as_adjacency_matrix(h)
hadjacency(h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
The adjacency matrix is a weighted adjacency matrix corresponding to
code(t(M) %*% M), where \code{M} is the incidence matrix, with the 
diagonal of the matrix set to 0. \code{hadjacency} is an alias for
the longer named function.
}
\value{
a (sparse) matrix.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{as_adjacency_matrix}},
\code{\link[Matrix]{Matrix}}.
}
\examples{
   h <- hypergraph_from_edgelist(list(1:4,2:5))
	hypergraph_as_adjacency_matrix(h)

# returns:
# 5 x 5 Matrix of class "dgeMatrix"
#   1 2 3 4 5
# 1 0 1 1 1 0
# 2 1 0 2 2 1
# 3 1 2 0 2 1
# 4 1 2 2 0 1
# 5 0 1 1 1 0

}
\keyword{ hypergraphs }
