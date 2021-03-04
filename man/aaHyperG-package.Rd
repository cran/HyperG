\docType{package}
\name{HyperG-package}
\alias{HyperG-package}
\alias{HyperG}
\title{
\packageTitle{HyperG}
}
\description{
\packageDescription{HyperG}
}
\section{Introduction}{
A graph is a set of vertices, V, and a set of egdes, E, 
each of which contains two
vertices (or a single vertex, if self-loops are allowed). A hypergraph is
a generalization of this, in which more than two vertices can be in a single
hyper-edge. Multi-graphs are graphs in which E is not a set, but rather allows
for duplicate edges. Hypergraphs are allowed to have duplicate hyper-edges.

\if{html}{\figure{h.png}{options: width=200 alt=""}}
\if{latex}{\figure{h.pdf}{options: width=1in}}

This package is a simple implementation of hypergraphs built around the
incidence matrix -- a binary matrix in which the rows correspond to 
the hyper-edges, the columns to vertices, and a 1 in position (i,j) indicates 
that the vertex j is in the ith hyper-edge. There is currently no support
for directed or weighted hypergraphs.

Various methods of manipulating hypergraphs, such as
adding and removing edges and vertices are implemented, and for small 
hypergraphs the \CRANpkg{igraph} package plot routine is used to plot
the hypergraph and its hyper-edges. For hypergraphs with more than a few
dozen vertices, it is recommended that the \code{plot} function be
called with \code{mark.groups=NULL}. See \code{\link[igraph]{igraph.plotting}}
for more information.

There are utilities in this package for removing loops, duplicate hyper-edges,
empty hyper-edges, and isolated vertices (ones that are not contained in
any hyper-edge). Also, there is a function, \code{reduce.hypergraph}, which
reduces the hypergraph down to its largest hyper-edges -- that is, it
removes hyper-edges that are subsets of other hyper-edges. It also
has other ways to reduce the hypergraph, see the corresponding manual
page.

There are also utilities for extracting information from the hypergraph.
For example, simple statistics such as the number of vertices, hyper-edges,
degrees of vertices, number of nodes per hyper-edge. Also global properties
such as
whether it is connected, if it has the Helly property or is conformal (see the
manual pages for \code{has.helly} and \code{is.conformal} for more information
on these topics).

}
\details{A hypergraph is implemented as a list containing (for now) a
single element, \code{M}, corresponding to the incidence matrix. 
It is an S3 object with class \code{hypergraph} and a plot method,
summary and print methods.
The package 
uses a sparse representation (from the \CRANpkg{Matrix} package), so
in principle it should allow for very large hypergraphs, although to 
date only relatively small hypergraphs have been investigated.


\packageIndices{HyperG}
}
\author{
\packageAuthor{HyperG}

Maintainer: \packageMaintainer{HyperG}
}
\note{
Some effort has been taken to avoid masking or redefining
functions from the \CRANpkg{igraph} package. While this results in awkward
function names ("hypergraph" nearly everywhere) it does reduce the chances
of hard-to-diagnose errors. I am considering adding aliases that 
replace "hypergraph" with "hg" or some such, but I'm not sure this is helpful.
The two functions that are masked, \code{is.simple} and \code{line.graph},
first check whether their argument is an \CRANpkg{igraph} graph, and if so
calls the corresponding \CRANpkg{igraph} function.
}
\references{
  Bretto, Alain, Hypergraph theory, An introduction. Springer, 2013.

  Voloshin, Vitaly I. Introduction to graph and hypergraph theory. 
  Nova Science Publ., 2009.
}
\seealso{
\code{\link[igraph:igraph-package]{igraph}}.
}
\examples{
h <- hypergraph_from_edgelist(list(1:2,2:5,3:7,c(1,3,5,7,9)))
hsize(h)
## 4
horder(h)
## 9
}
\keyword{ package }
\keyword{ hypergraphs }
