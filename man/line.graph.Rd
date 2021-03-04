\name{line.graph}
\alias{line.graph}
\title{
Line Graph
}
\description{
Construct the line graph of a hypergraph.
}
\usage{
line.graph(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
The line graph of a hypergraph is essentially the same concept as the
line graph of a graph: it is the graph whose vertices correspond to
the hyper-edges, with an edge between two vertices if their corresponding
hyper-edges intersect.
}
\value{
a graph.
}
\note{
If \code{h} is an \CRANpkg{igraph} graph, the function \code{igraph::line_graph}
will be called. \code{line.graph} is the only function that masks an
\CRANpkg{igraph} function, but I am assuming that the '_' version is
preferred by \CRANpkg{igraph}. 
}
\references{
  Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
  Nova Science Publ., 2009.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\seealso{
\code{\link[igraph]{line_graph}}.
}
\examples{
h <- hypergraph_from_edgelist(list(1:3,2:4,4:5,c(2,4:5)))
g <- line.graph(h)
}
\keyword{ hypergraphs }
