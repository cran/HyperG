\name{knn_hypergraph}
\alias{knn_hypergraph}
\title{
K-Nearest Neighbor Hypergraph.
}
\description{
A hypergraph is constructed from data in which each hyper-edge corresponds
to a vertex and its k-nearest neighbors.
}
\usage{
knn_hypergraph(x, k = 1, method = "Euclidean", reduce=FALSE,
   as.graph=FALSE)
}
\arguments{
  \item{x}{
a matrix of data points.
}
  \item{k}{
the number of neighbors. May be a vector.
}
  \item{method}{
distance type passed to \code{\link[proxy]{dist}}.
}
  \item{reduce}{
logical. Whether to remove redundant hyper-edges.
}
  \item{as.graph}{
logical. Whether to return a graph instead of a hypergraph.
}
}
\details{
Each vertex is in one-to-one correspondence with the points (rows) 
of \code{x}.
For each vertex, the k-closest vertices and itself form a hyper-edge.

If \code{reduce=TRUE} reduntant hyper-edges (those contained in other
hyper-edges) are removed. If \code{as.graph=TRUE}, \code{reduce} is
ignored and the incidence matrix is treated as an adjacency matrix,
returning a (directed) \CRANpkg{igraph} graph.
}
\value{
a hypergraph or graph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{epsilon_hypergraph}},
\code{\link[proxy]{dist}}.
}
\examples{
   set.seed(565)
	x <- matrix(rnorm(100),ncol=2)
	k <- knn_hypergraph(x,k=4)
	\donttest{
	   plot(k)
		plot(k,layout=x)
	}
}
\keyword{ hypergraph }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
