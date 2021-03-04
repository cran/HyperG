\name{hypergraph_laplacian_matrix}
\alias{hypergraph_laplacian_matrix}
\title{
Laplacian Matrix
}
\description{
The Laplacian of a hypergraph is D-A, where A is the (weighted) adjacency
matrix, and D is the row-sums.
}
\usage{
hypergraph_laplacian_matrix(h,normalize=FALSE)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{normalize}{
logical. Whether to normalize the Laplacian matrix.
}
}
\details{
The Laplacian is D-A, where D is the row sums of the adjacency matrix A.
If \code{normalize} is TRUE, then the normalized version is returned.
}
\value{
a (sparse) matrix.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{laplacian_matrix}}.
}
\examples{
 h <- hypergraph_from_edgelist(list(1:4,2:5,c(3,5,7,8,10),c(2,9),c(2:3,6,10)))
 L <- hypergraph_laplacian_matrix(h)
 Ln <- hypergraph_laplacian_matrix(h,normalize=TRUE)
}
\keyword{ hypergraphs }
