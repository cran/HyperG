\name{is.tree}
\alias{is.tree}
\alias{is.forest}
\title{
Test if a graph is a tree or a forest.
}
\description{
Test if a graph is a tree or a forest.
}
\usage{
is.tree(g)
is.forest(g,strict=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a graph.
}
  \item{strict}{
logical.
}
}
\details{
test whether an undirected graph \code{g} is a tree (connected, acyclic) or
a forest (disjoing union of trees). The flag \code{strict} enforces the 
strict rule that a forest must contain more than a single tree. The default
is to allow for single-tree forests, which is the convention.
}
\value{
a logical.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}

\examples{
   g <- make_tree(10)
	is.tree(g)
	is.forest(g)
	is.forest(g,strict=TRUE)
}
\keyword{ graphs }
