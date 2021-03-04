\name{is.hypergraph}
\alias{is.hypergraph}
\title{
Is an object a hypergraph?
}
\description{
Check that an object is a hypergraph object.
}
\usage{
is.hypergraph(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
This only checks that the object's class contains hypergraph.
}
\value{
A logical.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\examples{
   h <- hypergraph_from_edgelist(list(1:4,1:2,c(2,3,5),c(3,5:7)))
	g <- as.graph(h)
	is.hypergraph(h)
	is.hypergraph(g)
}
\keyword{ hypergraphs }
