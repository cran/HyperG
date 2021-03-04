\name{cluster_spectral}
\alias{cluster_spectral}
\title{
Spectral Graph Clustering
}
\description{
Use spectral embedding to embed a graph into a lower dimension, then cluster
the points using model based clustering. This results in a clustering of the
vertices.
}
\usage{
cluster_spectral(g, verbose = FALSE, adjust.diag = FALSE, laplacian = FALSE, 
   normalize = FALSE, scale.by.values = FALSE, vectors = "u", d = 12, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a graph.
}
  \item{verbose}{
logical. Whether to print to the screen as it goes.
}
  \item{adjust.diag}{
logical. Whether to set the diagonal of the adjacency matrix to
\code{degree/(n-1)}.
}
  \item{laplacian}{
logical. Whether to use the Laplacian rather than the adjacency matrix.
}
  \item{normalize}{
logical. Whether to normalize the matrix by \code{D^1/2}.
}
  \item{scale.by.values}{
Whether to scale the embedding vectors by the eigen vectors.
}
  \item{vectors}{
character. "u" or "v" or "uv". The latter is only appropriate for directed graphs.
}
  \item{d}{
embedding dimension.
}
  \item{\dots}{
arguments passed to \code{Mclust}.
}
}
\details{
This first embeds the vertices into a d-dimensional space, using the adjacency
matrix or the Laplacian. See \code{\link{ase}} for more information. It then
applies \code{\link[mclust]{Mclust}} to the resultant points to cluster.
}
\value{
An object of class "Mclust".
}
\references{
Fraley C. and Raftery A. E. (2002) Model-based clustering,
     discriminant analysis and density estimation, _Journal of the
     American Statistical Association_, 97/458, pp. 611-631.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\seealso{
\code{\link{ase}}.
}
\examples{
	P <- rbind(c(.2,.05),c(.05,.1))
	ns <- rep(50,2)
	set.seed(451)
	g <- sample_sbm(sum(ns),P,ns)
	cluster_spectral(g)
}
\keyword{ graph }
\concept{cluster the vertices of a graph}
\concept{model-based clustering}
