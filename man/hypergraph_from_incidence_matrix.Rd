\name{hypergraph_from_incidence_matrix}
\alias{hypergraph_from_incidence_matrix}
\alias{hypergraph_from_edgelist}
\alias{hypergraph_from_membership}
\alias{hypergraph_from_fuzzy_clustering}
\alias{hypergraph_from_spectral_clustering}
\title{
Hypergraph construction.
}
\description{
Construct a hypergraph from a collection of hyper-edges.
}
\usage{
hypergraph_from_incidence_matrix(incidence_matrix)
hypergraph_from_edgelist(x,v)
hypergraph_from_membership(x)
hypergraph_from_fuzzy_clustering(z,threshold)
hypergraph_from_spectral_clustering(g,m,fuzzy=FALSE,threshold,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incidence_matrix}{
an sxn binary matrix corresponding to the s hyper-edges on n vertices.
}
  \item{x}{
   a list of hyper-edges, or a vector corresponding to which hyper-edge
	each node is in.
}
  \item{z}{
   a matrix of probabilities that is nxc where c is the number of clusters.
}
  \item{v}{
optional vector of node names.
}
  \item{g}{
a graph. Only used if \code{m} is missing.
}
  \item{m}{
a communities object. See \link[igraph]{communities} in the \code{igraph}
package.
}
  \item{fuzzy}{
logical.
}
  \item{threshold}{
threshold on the probabilities if \code{fuzzy} is true. If not given,
it defaults to the inverse of the number of communities.
}
\item{\dots}{
arguments passed to \code{cluster_spectral} if \code{g} is given and 
\code{m} is given.
}
}
\details{
An edgelist is a list of hyper-edges. An incidence matrix is a binary
matrix that is \code{hsize(g)}x\code{horder(h)}. A membership vector
is the vector of node membership returned from a community detection
or clustering algorithm. A hypergraph constructed from a membership
vector has a disconnected component for each hyper-edge.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{cluster_spectral}},
\code{\link[igraph]{communities}}.
}

\examples{
   h <- hypergraph_from_edgelist(list(1:4,3:6))
}
\keyword{ hypergraphs }
\concept{ constructing hypergraphs from matrices }
