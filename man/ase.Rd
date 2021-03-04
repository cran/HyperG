\name{ase}
\alias{ase}
\alias{lse}
\alias{hypergraph.spectrum}
\title{
Adjacency spectral embedding.
}
\description{
Using either adjacency or Laplacian spectral embedding, embed a graph
into a lower dimensional space.
}
\usage{
ase(g, verbose = FALSE, adjust.diag = FALSE, laplacian = FALSE, 
    normalize = FALSE, scale.by.values = FALSE, vectors = "u", d = 2)
lse(g,...)
hypergraph.spectrum(h, k=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g,h}{
A graph (g) or hypergraph (h).
}
  \item{verbose}{
logical. Control output to terminal.
}
  \item{adjust.diag}{
logical. For adjacency embedding, whether to add \code{degree/(n-1)} to the 
diagonal of the adjacency matrix.
}
  \item{laplacian}{
logical. Use the Laplacian rather than the adjacency matrix.
}
  \item{normalize}{
logical. Whether to normalize by \code{D^1/2}.
}
  \item{scale.by.values}{
logical. Whether to scale the eigen or singular vectors by the square root
of the eigen or singular values.
}
  \item{vectors}{
character. "u", "v" or "uv" indicating which vectors to provide for the
embedding.
}
  \item{d,k}{
dimension of the embedding.
}
  \item{\dots}{
arguments passed to \code{ase}.
}
}
\details{
The \code{ase} is for graphs, and has the most control over the embedding,
as indicated by the arguments. \code{hypergraph.spectrum} computes the \code{svd} of
the incidence matrix for the hypergraph \code{h}.
\code{lse} is Laplacian spectral embedding, and is just a call to \code{ase}
with \code{laplacian=TRUE} and \code{adjust.diag=FALSE}.
For small hypergraphs (order or size < 3) the base svd function is used and k
is ignored.
}
\value{
\code{ase} returns a matrix of points, with rows corresponding to vertices
and columns to the embedding. There will be either d, or 2*d columns,
depending on the value of the variable \code{vectors}. For "u" or "v" the
dimension is \code{d}, for "uv" the dimension is \code{2*d}.
\code{hypergraph.spectrum} returns the singular value decomposition using the top
\code{k} singular vectors and values.
}
\references{
Congyuan Yang, Carey E. Priebe, Youngser Park, David J. Marchette, 
"Simultaneous Dimensionality and Complexity Model Selection for 
Spectral Graph Clustering," 
Journal of Computational and Graphical Statistics, 
accepted for publication, 2020. arXiv:1904.02926

A. Athreya, V. Lyzinski, D. J. Marchette, C. E. Priebe, D. L. Sussman, and M. Tang, "A limit theorem for scaled eigenvectors of random dot product graphs," Sankhya, vol. 78-A, no. 1, pp 1-18, February 2016.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}


\seealso{
\code{\link[RSpectra]{svds}},
\code{\link[RSpectra]{eigs}}.
}
\examples{
 g <- sample_gnp(10,.1)
 ase(g)
}
\keyword{ graph }
\concept{ functions to embed a graph into Euclidean space }
\concept{ functions to embed a hypergraph into Euclidean space }
