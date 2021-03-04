\name{sample_k_uniform_hypergraph}
\alias{sample_k_uniform_hypergraph}
\alias{sample_k_regular_hypergraph}
\title{
Random k-uniform and k-regular hypergraphs.
}
\description{
Randomly generate a hypergraph in which each hyper-edge contains k vertices,
or each vertex is incident to k hyper-edges.
}
\usage{
sample_k_uniform_hypergraph(n, m, k, prob)
sample_k_regular_hypergraph(n, m, k, prob)
}
\arguments{
  \item{n}{
the order of the hypergraph.
}
  \item{m}{
the size of the hypergraph.
}
  \item{k}{
the order of each hyper-edge.
}
  \item{prob}{
a vector of length \code{n} containing the probabilities for the vertices.
This is passed to \code{sample}.
}
}
\details{
A k-uniform hypergraph is one for which each hyper-edge contains exactly
k vertices. A k-regular hypergraph is one for which each vertex has degree k.
These are implemented through calls to \code{sample}.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}
\note{
For both of these functions \code{m}, the number of hyper-edges, must be
provided. This is unlike the corresponding functions for graphs -- all
simple graphs are 2-uniform, and there are constraints on the values of k
for which a graph can be k-regular, since the graphs are constrained to be
2-regular, i.e. all rows of the incidence matrix must contain 2 ones.
}

\seealso{
\code{\link{sample}}.
}
\examples{
set.seed(77)
h <- sample_k_uniform_hypergraph(20,5,3)
set.seed(73)
k <- sample_k_regular_hypergraph(20,5,3)
}
\keyword{ hypergraphs }
