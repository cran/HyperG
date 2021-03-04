\name{is.conformal}
\alias{is.conformal}
\alias{is.bi.conformal}
\title{
Conformal Hypergraphs
}
\description{
Tests whether a hypergraph is conformal.
}
\usage{
is.conformal(h)
is.bi.conformal(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
A hypergraph h is conformal if all the maximal cliques of its 2-section
are the maximal (by inclusion) edges of h. The test uses a theorem (see the
reference, Theorem 7.6.4) that says a hypergraph is conformal if and only if
its dual is Helly. A hypergraph is bi-conformal if it and its dual are conformal.
}
\value{
a logical.
}
\references{
Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
Nova Science Publ., 2009.
}
\author{
David J. Marchete \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{is.helly}}.
}
\examples{
h <- hypergraph_from_edgelist(list(1:4,3:6,5:10))
is.conformal(h)
## TRUE
h <- hypergraph_from_edgelist(list(1:2,2:3,c(1,3)))
is.conformal(h)
## FALSE
}
\keyword{ hypergraphs }
\concept{ Helly property }
