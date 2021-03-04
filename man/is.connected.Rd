\name{hypergraph.is.connected}
\alias{hypergraph.is.connected}
\title{
Is the hypergraph connected?
}
\description{
Uses the \CRANpkg{igraph} is.connected function to determine if a hypergraph is connected.
}
\usage{
hypergraph.is.connected(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
First the hypergraph is converted to a graph.
Then the resulting graph is passed to the \CRANpkg{igraph} \code{is.connected}
function.
}
\value{
a logical.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{is.connected}}.
}
\examples{
   hypergraph.is.connected(hypergraph_from_edgelist(list(1:4,3:5)))
	## TRUE
   hypergraph.is.connected(hypergraph_from_edgelist(list(1:4,5:7)))
	## FALSE
}
\keyword{ hypergraphs }
