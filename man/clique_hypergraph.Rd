\name{clique_hypergraph}
\alias{clique_hypergraph}
\title{
Clique Hypergraph
}
\description{
Construct a clique hypergraph from a graph.
}
\usage{
clique_hypergraph(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
A clique hypergraph is one whose hyper-edge correspond to the maximal
cliques of a given graph.
}
\value{
a hypergraph.
}
\references{
  Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
  Nova Science Publ., 2009.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\section{Warning}{
The calculation of the maximal cliques of a graph can take a long time,
and dense graphs may have many maximal cliques, so use this funciton with
care.
}

\seealso{
\code{\link[igraph]{max_cliques}}, ~~~
}
\examples{
   g <- graph_from_literal(1-2-3-1,3-4-5-3)
	h <- clique_hypergraph(g)
}
\keyword{ hypergraphs }
\keyword{ graphs }
\concept{ constructing hypergraphs from graphs }
