\name{kCores}
\alias{kCores}
\title{
K-Cores
}
\description{
Find all the k-cores in a hypergraph.
}
\usage{
kCores(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
A k-core in a hypergraph is a maximal subhypergraph where (a) no hyperedge 
is contained in another, and (b) each node is adjacent to at least \code{k} 
hyperedges in the subgraph.

The implementation is based on the algorithm by E. Ramadan, A. Tarafdar, A. Pothen, 2004.

The code is a direct copy of the code from the BioConductor package \code{hypergraph}, modified to work with the data structures used in this package.
}
\value{
A vector of core numbers for each vertex.
}
\references{
A hypergraph model for the yeast protein complex network,  Ramadan, E.   Tarafdar, A.   Pothen, A., Parallel and Distributed Processing Symposium, 2004. Proceedings. 18th International.

See also the BioConductor hypergraph package:

Seth Falcon and Robert Gentleman (2020). hypergraph: A package
  providing hypergraph data structures. R package version 1.62.0
}
\author{
David J. Marchette \email{dmarchette@gmail.com}

The implementation in the \code{hypergraph} package from which this function
was taken was written by:

Li Long \email{li.long@isb-sib.ch}.
}
\examples{
  ## example from the hypergraph version
  edges <- list(c("A", "C"), 
        c("B", "C"), 
		  c("C", "E"), 
		  c("C", "F"), 
		  c("E", "D"), 
		  c("E", "F"), 
		  c("D", "G"), 
		  c("D", "H"), 
		  c("D", "J"), 
		  c("H", "G"), 
		  c("H", "J"), 
		  c("G", "J"), 
		  c("J", "M"), 
		  c("J", "K"), 
		  c("M", "K"), 
		  c("M", "O"), 
		  c("M", "N"), 
		  c("K", "N"), 
		  c("K", "F"), 
		  c("K", "I"), 
		  c("K", "L"), 
		  c("F", "I"), 
		  c("I", "L"), 
		  c("F", "L"), 
		  c("P", "Q"), 
		  c("Q", "R"), 
		  c("Q", "S"), 
		  c("R", "T"), 
		  c("S", "T"))
	h <- hypergraph_from_edgelist(edges,v=union(unlist(edges),"U"))
	kc <- kCores(h)

kCores(h)
}
\keyword{ hypergraphs }
