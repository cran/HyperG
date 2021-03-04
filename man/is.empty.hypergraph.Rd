\name{is.empty.hypergraph}
\alias{is.empty.hypergraph}
\title{
Is the hypergraph empty.
}
\description{
determines whether the hypergraph contains no hyper-edges.
}
\usage{
is.empty.hypergraph(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\value{
a logical.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\examples{
  h <- hypergraph_from_edgelist(list(1:3))
  is.empty.hypergraph(h)
  k <- hypergraph.delete.edges(h,1)
  is.empty.hypergraph(k)
}
\keyword{ hypergraphs }
