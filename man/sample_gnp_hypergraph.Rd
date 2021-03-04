\name{sample_gnp_hypergraph}
\alias{sample_gnp_hypergraph}
\title{
Erdos-Renyi hypergraphs.
}
\description{
Sample an Erdos-Renyi p hypergraph.
}
\usage{
sample_gnp_hypergraph(n, m, p, lambda)
}
\arguments{
  \item{n}{
number of nodes.
}
  \item{m,lambda}{
controls the number of hyper-edges. If m is not given, 
the number is drawn from a Poisson(lambda), or, a Poisson(n*p) if lambda
is not given.
}
  \item{p}{
Hyper-edge probability.
}
}
\details{
This generates an ER hypergraph by using \code{\link{rbinom}}
to generate a random \code{mxn} matrix of Bernoulli random variables and
treating this 
matrix as the incidence matrix for the hypergraph. 
If \code{m} is not given, and \code{lambda} is, then m is drawn from a 
Poisson distribution with parameter \code{lambda}. If neither is given,
the number of hyper-edges is drawn from a Poisson distribution with
parameter \code{n*p}.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}


\seealso{
\code{\link{rbinom}},
\code{\link{rpois}}.
}
\examples{
	set.seed(672)
   h <- sample_gnp_hypergraph(n=100,p=.1)
}
\keyword{ hypergraphs }
