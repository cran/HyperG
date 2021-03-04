\name{reorder_vertices}
\alias{reorder_vertices}
\title{
Reorder the vertices of a hypergraph.
}
\description{
Return a hypergraph in which the vertices have been reordered so that they
are in the given order as columns of the incidence matrix.
}
\usage{
reorder_vertices(h,ord,decreasing=FALSE)
}
\arguments{
  \item{h}{
a hypergraph.
}
  \item{ord}{
  an ordering of the vertices. 
}
  \item{decreasing}{
if \code{ord} is not given, the order function is called on the names
of the vertices as controlled by the \code{decreasing} variable.
}
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{order}}
}
\examples{
   h <- hypergraph_from_edgelist(list(3:7,8:12,c(1,3,9)))
	k <- reorder_vertices(h)
	hnames(h)
	hnames(k)

   h <- hypergraph_from_edgelist(list(letters[3:7],letters[8:12],
	                                   LETTERS[c(1,3,9)]))
	k <- reorder_vertices(h)
	hnames(h)
	hnames(k)

}

\keyword{ hypergraphs }
