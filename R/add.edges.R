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

hypergraph.add.edges <- function(h,edges,verbose=FALSE)
{
	if(missing(edges) || is.null(edges)) {
	   M <- incidence_matrix(h)
		M <- rbind(M,rep(0,horder(h)))
		return(hypergraph_from_incidence_matrix(M))
	}
	hv <- hnames(h)
	ev <- unique(unlist(edges))
	## handle the case where indices are given instead of vertex names
	if(is.numeric(ev)){
	   x <- setdiff(ev,1:horder(h))
		## handle the case of vertices referenced that aren't in the graph
		if(!is.null(x)){
		   h <- hypergraph.add.vertices(h,length(x),names=as.character(x))
			if(verbose){
				warning(paste(length(x),"vertices added to the graph"))
		   }
		}
		for(i in 1:length(edges)){
		   edges[[i]] <- hnames(h)[match(edges[[i]],hnames(h))]
		}
		hv <- hnames(h)
	}
	## handle the case of vertices referenced that aren't in the graph
	if(!all(ev %in% hv)){
	   v <- setdiff(ev,hv)
		h <- hypergraph.add.vertices(h,length(v),names=v)
			if(verbose){
				warning(paste(length(v),"vertices added to the graph"))
			}
	}
	M <- Matrix::Matrix(0,nrow=length(edges),
							  ncol=horder(h),dimnames=list(NULL,hnames(h)))
	for(i in 1:length(edges)){
		M[i,edges[[i]]] <- 1
	}
	M <- rbind(h$M,M)
	hypergraph_from_incidence_matrix(M)
}


add.hyperedges <- hypergraph.add.edges
