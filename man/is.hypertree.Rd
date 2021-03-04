\name{is.hypertree}
\alias{is.hypertree}
\title{
Test for hypertree.
}
\description{
Test if a hypergraph is a hypertree.
}
\usage{
is.hypertree(h, ...)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{\dots}{
arguments passed to the \CRANpkg{igraph} 
\code{\link[igraph]{is_chordal}} function.
}
}
\details{
Uses Corollary 8.1.1 of the reference: a hypergraph is a hypertree if
and only if it is Helly and it's line graph is chordal.
}
\value{
a logical.
}
\references{
  Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
  Nova Science Publ., 2009.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}


\seealso{
\code{\link[igraph]{is_chordal}}.
\code{\link[igraph]{line_graph}}.
\code{\link{line.graph}},
\code{\link{has.helly}}.
}
\examples{
h <- hypergraph_from_edgelist(list(1:3,2:4,4:5,c(2,4:5)))
is.helly(h)
g <- line.graph(h)
is_chordal(g)
is.hypertree(h)
}
\keyword{ hypergraph }
\keyword{ graphs }
