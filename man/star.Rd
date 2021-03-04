\name{is.star}
\alias{is.star}
\alias{intersection_set}
\title{
Is a hypergraph a star?
}
\description{
Tests whether a hypergraph is a star. Finds the minimal intersection set
of the hyper-edges.
}
\usage{
is.star(h,type=c("weak","strong"))
intersection_set(h)
}
\arguments{
  \item{h}{
a hypergraph. 
}
  \item{type}{
see Details.
  }
}
\details{
A (weak) star hypergraph is one in which the intersection of all the hyper-edges
is non-empty.

An intersection set is a set of vertices that is contained in every
edge.
The argument \code{type="strong"} for \code{is.star} 
indicates that the only vertices which
are common between any pair of vertices are contained in the intersection set
(or "hub") of the star hypergraph. So a hypergraph that is strongly
star is weakly star, but not vice versa.

}
\value{
a logical, for \code{is.star}. A set of vertices (or NULL) that are contained
in every hyper-edge. This would be the "hub" of the star.
}
\references{
Akram, M., & Luqman, A. (2020). Fuzzy hypergraphs and related extensions. 
Springer Singapore.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}.
}
\examples{
h <- hypergraph_from_edgelist(list(c(1:4,16),
		  c(4:7,16),
		  c(4,8:10,16),
		  c(4,10:16),
		  c(4,16)))
is.star(h)  ## TRUE
is.star(h,type='strong')  ## FALSE
intersection_set(h)     ## 4, 16
}
\keyword{ hypergraphs }
