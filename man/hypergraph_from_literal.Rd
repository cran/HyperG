\name{hypergraph_from_literal}
\alias{hypergraph_from_literal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hypergraph from literal.
}
\description{
Similar to the \CRANpkg{igraph} function, except that in this case the literals just indicate the hyper-edges.
}
\usage{
hypergraph_from_literal(...)
}
\arguments{
  \item{\dots}{
see details.
}
}
\details{
This takes a collection of hyperedge descriptions, such as a-3-C-9, indicating
the hyper-edge containing a,C,3 and 9. It returns the associated hypergraph.
This is different from the graph version; in essence, this would be the
same as constraining the graph version to require all the argumenst to be
pairs. There is no way to produce chains, as in the graph version.
}
\value{
a hypergraph.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link[igraph]{graph_from_literal}}.
}
\examples{
   h1 <- hypergraph_from_literal(1-2-3,3-a-b,c-1-4)
	\donttest{
	   plot(h1)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{ hypergraphs }
\concept{ constructing hypergraphs }
