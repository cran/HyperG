\name{make_empty_hypergraph}
\alias{make_empty_hypergraph}
\title{
Empty hypergraph.
}
\description{
Create an empty hypergraph.
}
\usage{
make_empty_hypergraph(n)
}
\arguments{
  \item{n}{
a non-negative integer.
}
}
\details{
Creates an empty hypergraph (no hyper-edges) on \code{n} nodes.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
   h <- make_empty_hypergraph(4)
	horder(h)
	hsize(h)
	h <- make_empty_hypergraph(0)
	horder(h)
	hsize(h)
}
\keyword{ hypergraphs }
