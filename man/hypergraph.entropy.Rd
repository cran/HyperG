\name{hypergraph.entropy}
\alias{hypergraph.entropy}
\title{
Hypergraph Entropy
}
\description{
The hypergraph entropy, which is a sum of the suitably scaled eigenvalues
of the hypergraph Laplacian.
}
\usage{
hypergraph.entropy(h)
}
\arguments{
  \item{h}{
a hypergraph.
}
}
\details{
Bretto, page 9, defines hypergraph entropy as follows. Let L'(h) be the Laplacian
of h
divided by the sum of its diagonal. Then the \eqn{|V|-1}{|V|-1} eigenvalues
sum to 1, and the entropy is defined by 
\eqn{-sum(\lambda_i\log_2\lambda_i)}{-sum ev[i]*log2 ev[i]}.
}
\value{
a number.
}
\references{
  Bretto, Alain, Hypergraph theory, An introduction. Springer, 2013.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{hypergraph_laplacian_matrix}}.
}
\examples{
h <- hypergraph_from_edgelist(list(3:4,1:3,c(3,5,7:10),c(4,6),c(3,5,8)))
hypergraph.entropy(h) 
## 2.802822
}
\keyword{ hypergraphs }
