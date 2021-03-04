\name{plot.hypergraph}
\alias{plot.hypergraph}
\title{
Plot a hypergraph.
}
\description{
Plot a hypergraph using the \CRANpkg{igraph} plot function.
}
\usage{
\method{plot}{hypergraph}(x, edge.color = NA, 
	 mark.groups=hypergraph_as_edgelist(h),
    layout,...)
}
\arguments{
  \item{x}{
a hypergraph.
}
  \item{edge.color}{
color for the edges.
}
  \item{layout}{
optional layout for the plot. If the hypergraph has a layout attribute, this
will be used, unless layout is given. if it does not have a layout attribute
and the layout is not provided, it uses the code from \CRANpkg{igraph} to choose
a layout.
}
\item{mark.groups}{
   the groups correspond to the hyper-edges. Set this to NULL if you do not
	want the hyper-edge polygons to plot.
}
  \item{\dots}{
optional arguments passed to plot.
}
}
\details{
Plots the hypergraph, using the \CRANpkg{igraph} plotting function applied
to a graph converted from the hypergraph.
For the example below, the plot will look something like: 

\if{html}{\figure{h.png}{options: width=640 alt=""}}
\if{latex}{\figure{h.pdf}{options: width=3in}}
}
\value{
the layout is returned invisibly.
}
\author{
David J. Marchette \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{as.graph}},
\code{\link[igraph]{plot.igraph}},
\code{\link[igraph]{igraph.plotting}}.
}
\examples{
   h <- hypergraph_from_edgelist(list(3:7,8:12,c(1,3,9)))
\donttest{
   plot(h)
}
}
\keyword{ hypergraphs }
\keyword{ graph }
