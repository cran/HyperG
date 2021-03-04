\name{hdegree}
\alias{hdegree}
\alias{plotDegreeDistribution}
\title{
Degrees of a hypergraph.
}
\description{
The degree of a vertex in a hypergraph is the number of hyper-edges containing
the vertex.
}
\usage{
hdegree(h)
plotDegreeDistribution(h, xlab="Degree",
	ylab="Density",
	add.line=FALSE,
	lty=2,lwd=1,line.col=1,
	...)
}
\arguments{
  \item{h}{
a hypergraph. For \code{plotDegreeDistribution} it could also be a graph.
}
  \item{xlab,ylab}{
axis labels.
}
  \item{add.line}{
logical. Whether to add a regression line to the plot.
}
  \item{lty,lwd,line.col}{
plotting controls for the line.
}
  \item{\dots}{
arguments passed to \code{plot}.
}
}
\details{
Returns a vector of the number of (hyper-)edges containing each vertex.
The \code{plot.hdegree.distributions} plots the distribution of degrees on a
log-log scale, optionally adding a regression line.
}
\value{
a vector of degrees.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\seealso{
\code{\link[igraph]{degree}},
\code{\link[igraph]{degree.distribution}}.
}
\examples{
   set.seed(452)
	h <- sample_gnp_hypergraph(100,p=.1)
	hdegree(h)
}
\keyword{ hypergraphs }
