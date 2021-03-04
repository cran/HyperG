\name{incidence_matrix}
\alias{incidence_matrix}
\alias{hypergraph_as_incidence_matrix}
\title{
Graph Incidence Matrix.
}
\description{
Converts a graph to an incidence matrix. Not the bipartite version.
}
\usage{
incidence_matrix(g)
hypergraph_as_incidence_matrix(h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a graph or hypergraph.
}
  \item{h}{
a hypergraph.
}
}
\details{
An incidence matrix has \code{gorder(g)} columns and \code{gsize(g)}
rows. \code{incidence_matrix} can be called on either a graph or a
hypergraph. It calls \code{hypergraph_as_incidence_matrix} in the 
latter case.
}
\value{
A sparse incidence matrix.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\examples{
	set.seed(2343)
   g <- sample_gnp(10,.1)
	h <- hypergraph_from_edgelist(list(1:3,3:4,c(3,5,7)))
	\dontshow{
	incidence_matrix(g)
	as_adjacency_matrix(g)
	hypergraph_as_incidence_matrix(h)
	}
}
\keyword{ hypergraphs }
