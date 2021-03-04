\name{dual}
\alias{dual_hypergraph}
\title{
Dual hypergraph.
}
\description{
Construct the dual hypergraph of a hypergraph.
}
\usage{
dual_hypergraph(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
The dual hypergraph is a hypergraph whose nodes are the original hyper-edges,
with hyper-edges indicating the original incidence. Essentially, the 
incidence matrix of the dual hypergraph is the transpose of the original 
incidence matrix. 

}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\references{
  Tyshkevich, R.I. and Zverovich, Vadim E,
  Line hypergraphs,Discrete Mathematics,
  161, 265--283,1996.
}

\examples{
   h <- hypergraph_from_edgelist(list(1:4,2:5,4:6,c(1,3,7)))
	k <- dual_hypergraph(h)
}
\keyword{ hypergraphs }
\keyword{ graph }
