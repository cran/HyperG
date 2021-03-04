\name{pendant}
\alias{pendant}
\title{
Pendant Vertices
}
\description{
Determine the set of pendant vertices.
}
\usage{
pendant(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
A pendant vertex is one whose set of hyper-edges is a subset of the
hyper-edges of another vertex. That is, if v is in hyper-edges 1 and 2, and
w is in hyper-edges 1, 2 and 5, then v is pendant to w, and w is 
called a twin of v.
}
\value{
a list containing:
\item{vertices }{a vector of the pendant vertices}
\item{twins }{a list, each element of which is the set of twins of the
corresponding pendant vertex}
}
\references{
  Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
  Nova Science Publ., 2009.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}

\examples{
h <- hypergraph_from_edgelist(list(1:4,2:5,c(4,6),c(7),c(3:5,8)))
pendant(h)
}
\keyword{ hypergraphs }
