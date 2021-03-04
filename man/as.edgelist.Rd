\name{hypergraph.as.edgelist}
\alias{hypergraph_as_edgelist}
\alias{hyper_edges}
\title{
Convert between hypergraphs and graphs.
}
\description{
Convert a hypergraph to a graph or a graph, matrix or list to a hypergraph.
}
\usage{
hyper_edges(h)
hypergraph_as_edgelist(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
The function \code{hypergraph_as_edgelist} is just an alias for 
\code{hyper_edges}. 

}
\value{
A list of the hyperedges.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\seealso{
\code{\link[igraph]{as_edgelist}}
}
\examples{
   h <- hypergraph_from_edgelist(list(1:4,1:2,c(2,3,5),c(3,5:7)))
	hypergraph_as_edgelist(h)
}
\keyword{ hypergraphs }
\keyword{ graph }
\concept{functions for manipulating hypergraph structure}
