\name{equivalent.hypergraphs}
\alias{equivalent.hypergraphs}
\alias{as.binary.hypergraph}
\title{
Equivalent Hypergraphs
}
\description{
Test whether two hypergraphs are equivalent. This is not an isomorphism test, merely a test
that the incidence matrices are "the same" in the vertex/edge order in which they are 
resented.
}
\usage{
equivalent.hypergraphs(h1, h2, vertex.names = FALSE, edge.names = FALSE, 
	strip.names=FALSE,
   method = c("any", "exact", "binary"))
as.binary.hypergraph(h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h,h1,h2}{
hypergraphs.
}
  \item{vertex.names}{
logical. Whether to ensure the vertex names are all the same.
}
  \item{edge.names}{
logical. Whether to ensure the hyper-edge names are all the same.
}
  \item{strip.names}{
logical. Whether to strip the row/column names from the incidence matrices (after ordering
them) prior to the equivalence check.
}
  \item{method}{
see Details.
}
}
\details{
If either \code{vertex.names} or \code{edge.names} is TRUE, they are checked for equality,
and the incidence matrices are reordered accordingly. The method "exact" checks for 
the matrices being exactly equal, while "binary" converts all non-zero entries to 1 before
the check. The former is for future versions in the event that weighted or directed
hypergraphs are implemented. 
If \code{strip.names} is TRUE, the row/column names are stripped from the matrices. If
either \code{vertex.names} or \code{edge.names} is TRUE, the matrices are first ordered
according to the rows/columns as appropriate.
Note that "binary" will always be TRUE if "exact" is TRUE, and that if "binary" is FALSE,
then so will "exact" be.

The method "any" calls the code with \code{strip.names=TRUE} and all combinations of
\code{vertex.names} and \code{edge.names}
for the binary
method, and returns the logical OR of these. Essentially, this tests that the matrices are
"the same" under any reasonable interpretation (without checking for equivalence under
any reordering except for lexicographic ordering of the row/column names).
}
\value{
a logical in the case of \code{equivalent.hypergraphs}, a hypergraph in the case of
\code{as.binary.hypergraph}.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
    h1 <- hypergraph_from_edgelist(list(1:4,2:7,c(1,3,5,7),c(2,4,6)))
    h2 <- hypergraph_from_edgelist(list(letters[1:4],letters[2:7],
	           letters[c(1,3,5,7)],letters[c(2,4,6)]))
    equivalent.hypergraphs(h1,h2)  ## TRUE
    equivalent.hypergraphs(h1,h2,vertex.names=TRUE)  ## FALSE
}
\keyword{ hypergraphs }
