## HyperG R Package
## 
## Copyright (c) 2021 David J. Marchette <dmarchette@gmail.com>
## 
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
## 
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
## 
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
## 

graph2hypergraph <-
function(g,
									 method=c("incidence",
									          "adjacency",
												 "neighborhood",
												 "ego",
                                     "spectral"
												 ),
									 ...)
{
	m <- from_igraph.match.arg(method)
	if(is.null(m)) stop("invalid method")
   switch(m,
		incidence = hypergraph_from_incidence_matrix(incidence_matrix(g)),
	   adjacency = hypergraph_from_incidence_matrix(as_adjacency_matrix(g)),
		neighborhood = hypergraph_from_edgelist(ego(g,...)),
		ego = hypergraph_from_edgelist(ego(g,...)),
		spectral = hypergraph_from_spectral_clustering(g,...)
		)
	
}

as.hypergraph <- function(x,n,...) 
{
	if(missing(x) || is.null(x)){
	   if(missing(n) || n<1) stop("must provide x or a positive n")
		return(make_empty_hypergraph(n))
	}
	if(inherits(x,'dgCMatrix')){
		return(hypergraph_from_incidence_matrix(x))
	}
	if(inherits(x,"simple_triplet_matrix")){
	   sparsex <-  Matrix::sparseMatrix(i=x$i, j=x$j,
										dimnames=dimnames(x),
                              x=x$v,dims=c(x$nrow, x$ncol))
	   return(hypergraph_from_incidence_matrix(sparsex))
	}
   if(is.matrix(x)) {
		return(hypergraph_from_incidence_matrix(x))
	}
	if(is.igraph(x)) {
		return(graph2hypergraph(x,...))
	}
	if(is.list(x)){
		return(hypergraph_from_edgelist(x))
	}
	if(!missing(n) && n>0) {
		warning("x unrecognized. returning empty hypergraph.")
		return(make_empty_hypergraph(n))
	}
	warning("x unrecognized. returning empty hypergraph on 0 nodes.")
	make_empty_hypergraph(0)
	
}
