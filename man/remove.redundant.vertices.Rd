\name{remove.redundant.vertices}
\alias{remove.redundant.vertices}
\title{
Remove redundant vertices.
}
\description{
Remove vertices whose removal does not disconnect the hypergraph. 
}
\usage{
remove.redundant.vertices(H, check.empty = TRUE)
}
\arguments{
  \item{H}{
a hypergraph.
}
  \item{check.empty}{
logical. See details.
}
}
\details{
This function was created with a binary term document hypergraph in mind.
The idea is to remove words that appear in large documents, in order
of decreasing degree, so long as the removal does not disconnect the
graph. The argument \code{check.empty} is to ensure that removing a word
does not result in an empty document. If this is set to FALSE, the resultant
hypergraph may be much smaller than expected.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\examples{
  edges <- list(letters[c(1:4,9)],
					 letters[3:9],
					 letters[c(3,6:9)],
					 letters[c(3,5:9)],
					 letters[2:9])
  h1 <- hypergraph_from_edgelist(edges)
  h <- remove.redundant.vertices(h1)
  # removed c, f, g, i

  h2 <- hypergraph_from_edgelist(list(letters[1:3],
					  letters[3:5]))

  h <- remove.redundant.vertices(h2)
  ## h == h2
}
\keyword{ hypergraphs }
