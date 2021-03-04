\name{sample_sbm_hypergraph}
\alias{sample_sbm_hypergraph}
\title{
Sample from a stochastic block model.
}
\description{
A stochastic block model hypergraph.
}
\usage{
sample_sbm_hypergraph(n,P,block.sizes,d,impurity=0,variable.size=FALSE,
   absolute.purity=TRUE)
}
\arguments{
  \item{n}{
  number of vertices.
 }
  \item{P}{
A kxk probability matrix.
}
  \item{block.sizes}{
vector of community sizes.
}
  \item{d}{
size of a hyper-edge. See Details.
}
  \item{impurity}{
See Details.
}
  \item{variable.size,absolute.purity}{
logical. See Details.
}
}
\details{
A stochastic
block model is first generated using the function 
\code{\link[igraph]{sample_sbm}(n,P,block.sizes)}. The edges are
augmented with vertices, resulting in a stochastic block model hypergraph,
as discussed below.

The variable 
\code{d} corresponds to the number of vertices per edge. If it is a vector,
it is recycled as necessary. If \code{variable.size} is TRUE, then d is used
as the mean of a Poisson random variable to generate hyper-edge orders,
to which 2 is added. So a \code{d} of 2 will result in hyper-edge orders
with a mean of 4.

For each edge (say, edge k) in the graph, 
new vertices are added so that the number of
vertices in the (now hyper-)edge is d[k].

If \code{impurity} is 0, then for each edge
the vertices are added in proportion
to the block sizes, using \code{sample}, 
so if one community has many more vertices than the others,
it will tend to dominate in the hyper-edges as well. However, it is guaranteed
that hyper-edges between two distince communities have at least one vertex
from each of those two communities, and hyper-edges within communities are pure
in the case of \code{impurity=0};
no hyper-edge will contain vertices from more than two communities.

If \code{impurity>0}, then \code{impurity} of the vertices not
in the original stochastic block model hypergraph 
are replaced by random vertices. If \code{absolute.purity} is TRUE, these
new vertices are
sampled from all other classes. Otherwise they are sample from vertices not
in the original hyper-edge. If \code{k} is 2, only the within community
hyper-edges will contain impurities.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\seealso{
\code{\link{sample_gnp_hypergraph}}, \code{\link[igraph]{sample_sbm}}.
}
\examples{
	\donttest{
	P <- rbind(c(0.1,0.01),c(0.01,0.1))
	block.sizes <- c(50,50)
	set.seed(55)
   h <- sample_sbm_hypergraph(100,P=P,block.sizes=block.sizes,d=4)
	range(edge_orders(h))
	## should all be 4
	set.seed(1233)
	k <- sample_sbm_hypergraph(100,P=P,d=2,block.sizes=block.sizes,
	             variable.size=TRUE)
	mean(edge_orders(k))
	## should be approximately 4
	set.seed(1235)
	Q <- rbind(c(.2,.01,.01),
	           c(.01,.1,.05),
				  c(.01,.05,.2))
	kk <- sample_sbm_hypergraph(300,P=Q,d=6,block.sizes=rep(100,3),
	             variable.size=TRUE,impurity=2)

	}
}
\keyword{ hypergraphs }
