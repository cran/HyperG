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

is.tree <- function(g)
{
   (no.clusters(g)==1) && (gsize(g)==(gorder(g)-1))
}

is.forest <- function(g,strict=FALSE)
{
   comp <- components(g)
	if(strict && (length(comp)==1)) return(FALSE)
	for(i in 1:length(comp)){
	   if(!is.tree(induced_subgraph(g,comp[[i]]))) return(FALSE)
	}
	TRUE
}

is.hypertree <- function(h,...)
{
	if(!is.helly(h)) return(FALSE)
   is_chordal(line.graph(h),...)$chordal
}

subtree.hypergraph <- function(g,v)
{
	n <- gorder(g)
	if(n<2) stop("g must be a non-trivial tree")
	if(missing(v)){
	   vertices <- NULL
		edges <- vector('list',1)
		k <- 3
		for(i in 1:(n-1)){
			v <- setdiff(n:1,vertices)
			j <- setdiff(v[1:min(k,length(v))],i)
			if(length(j)==0) j <- i+1
		   newEdge <- unique(as.vector(unlist(all_simple_paths(g,i,j))))
			vertices <- union(vertices,newEdge)
			if(length(vertices)==n) break
			edges[[i]] <- newEdge
		}
		## special case for paths
		if(length(edges)==1){
		   edges <- list(1:floor(n/2),floor(n/2):n)
		}
		h <- hypergraph_from_edgelist(edges)
	} else {
		edges <- lapply(v,function(x) 
		      unique(as.vector(unlist(all_simple_paths(g,x[1],x[-1])))))
		h <- hypergraph_from_edgelist(edges)
		vertices <- unique(unlist(edges))
		missingVs <- setdiff(1:n,vertices)
		while(length(missingVs)>0){
			v <- missingVs[1]
			x <- as.numeric(hnames(h))
			newEdge <- unique(as.vector(unlist(all_simple_paths(g,v,x[1]))))
			h <- hypergraph.add.edges(h,list(newEdge))
			missingVs <- setdiff(missingVs,newEdge)
		}
	}
	h
}

