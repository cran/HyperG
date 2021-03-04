\name{has.helly}
\alias{has.helly}
\alias{is.helly}
\title{
Helly Property
}
\description{
Check whether a hypergraph has the Helly property.
}
\usage{
has.helly(h, strong=FALSE)
is.helly(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{strong}{
logical.
}
}
\details{
An intersecting family is a collection of hyper-edges such that the intersection
of any pair of hyper-edges in the family is
non-empty. A hypergraph has the Helly property if each intersecting family 
has a non-empty intersection -- there is at least one vertex in every
hyper-edge. This is an implementation of the algorithm on page 32 of
Bretto. The argument \code{strong} indicates whether the test should be
for the strong Helly property or not. A hypergraph has the strong Helly
property if every partial induced sub-hypergraph has the Helly property.

The function \code{is.helly} is an alias for 
a check for the non-strong Helly property.
}
\value{
a logical.
}
\references{
  Bretto, Alain, Hypergraph theory, An introduction. Springer, 2013.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\note{
Have not yet implemented the strong Helly property algorithm.
}

\examples{
	## Example from Bretto
	h <- hypergraph_from_edgelist(list(1:5,
	          c(2,4,6,7),
				 c(4:6,8,9),
				 9:10))
   has.helly(h)
}
\keyword{ hypergraphs }
\concept{Helly property}
