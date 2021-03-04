\name{epsilon_hypergraph}
\alias{epsilon_hypergraph}
\title{
Epsilon-Ball Hypergraph
}
\description{
Build a hypergraph by constructing hyperedges from balls around a set of points.
}
\usage{
epsilon_hypergraph(x, epsilon, method = "Euclidean", reduce=FALSE, 
   as.graph=FALSE)
}
\arguments{
  \item{x}{
a matrix of points.
}
  \item{epsilon}{
radius of the balls. May be a vector.
}
  \item{method}{
passed to \code{\link[proxy]{dist}} to define the distance function.
}
  \item{reduce}{
logical. Whether to reduce the hypergraph by removing reduntant hyper-edges.
}
  \item{as.graph}{
logical. Whether to return a graph instead of a hypergraph.
}
}
\details{
Each point of \code{x} corresponds to a vertex in the hypergraph.
For each point, a ball of radius \code{epsilon} is constructed,
and all points in the ball form a hyper-edge in the graph.
If epsilon is a vector, each ball may have a different radius, and
if the length of epsilon is less than the number of points, they are
repeated.

If \code{reduce=TRUE} reduntant hyper-edges (those contained in other
hyper-edges) are removed. If \code{as.graph==TRUE}, \code{reduce} is
ignored an a graph is returned instead of a hypergraph.
}
\value{
a hypergraph or graph.
}
\note{
Because of symmetry (a is in the ball centered at b if and only if b is in the 
ball centered at a), the incidence matrix of an epsilon hypergraph is
square and symmetric. It can thus be interpreted as an adjacency matrix,
and it is this graph that is returned if \code{as.graph==TRUE}.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}
\seealso{
\code{\link{knn_hypergraph}},
\code{\link{sample_geom_hypergraph}},
\code{\link[proxy]{dist}}.
}
\examples{
   set.seed(565)
	x <- matrix(rnorm(100),ncol=2)
	h <- epsilon_hypergraph(x,epsilon=.25)
	\donttest{
	   plot(h)
		plot(h,layout=x)
	}
	epsilons <- runif(nrow(x),0,.5)
	k <- epsilon_hypergraph(x,epsilon=epsilons)
	\donttest{
	   plot(k)
		plot(k,layout=x)
	}
}
\keyword{ hypergraphs }
