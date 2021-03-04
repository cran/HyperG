\name{H2}
\alias{H2}
\title{
Two sections of a hypergraph.
}
\description{
Two section of a hypergraph.
}
\usage{
H2(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
The 2-section of a hypergraph is the graph with vertices corresponding to hyper-edges,
and edges corresponding to whether the hyper-edges intersect. 

}
\value{
a graph.
}
\references{

  Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
  Nova Science Publ., 2009.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
h <- hypergraph_from_edgelist(list(c(1,2,5),c(2,3,5),c(3,4),c(4,5)))
g <- H2(h)
## see Figure 7.11 of the reference.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ hypergraphs }
\concept{ two-sections }
