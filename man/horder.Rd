\name{horder}
\alias{hnames}
\alias{horder}
\alias{hsize}
\alias{edge_orders}
\title{
The number of vertices, edges and statistics of the hypergraph.
}
\description{
This returns the number of vertices and hyper-edges, and similar statistics,
for a hypergraph.
}
\usage{
hnames(h)
horder(h)
hsize(h)
edge_orders(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\value{
a named vector of vertics, or the names of the vertices.
Order refers to the number of vertices, size to the number of hyper-edges.
The \code{edge_orders} function returns the number of vertices in each of the
hyper-edges. In a simple graph, this would always be 2.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{gorder}},
\code{\link[igraph]{gsize}},
\code{\link{hrank}}.
}
\examples{
   h <- hypergraph_from_edgelist(list(3:7,8:12,c(1,3,9)))
	horder(h)
	hsize(h)
	hnames(h)
}

\keyword{ hypergraphs }
