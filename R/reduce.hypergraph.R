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

reduce.hypergraph <- function(h,method="inclusion")
{
   if(method=="intersection"){
	   return(reduce.hypergraph.intersection(h))
   } else if(method=="inclusion"){
	   return(reduce.hypergraph.inclusion(h))
	} else if(method=="union"){
	   return(reduce.hypergraph.union(h))
	}
}

reduce.hypergraph.inclusion <-
function(h)
{
   remove <- rep(FALSE,hsize(h))
	edges <- hypergraph_as_edgelist(h)
	if(!is.null(edges)){
		for(i in 1:(length(edges)-1)){
			if(!remove[i]){
			ei <- edges[[i]]
				for(j in (i+1):length(edges)){
					if(!remove[j]){
						ej <- edges[[j]]
						if(all(ei %in% ej)) remove[i] <- TRUE
						else if(all(ej %in% ei)) remove[j] <- TRUE
					}
				}
			}
		}
	} else {
	   return(h)
	}
	as.hypergraph(edges[!remove],n=horder(h))
}

reduce.hypergraph.intersection <-
function(h)
{
	edges <- hypergraph_as_edgelist(h)
	if(!is.null(edges)){
		new_edges <- vector('list',0)
		k <- 0
		for(i in 1:(length(edges)-1)){
			ei <- edges[[i]]
			for(j in (i+1):length(edges)){
				ej <- edges[[j]]
				int <- intersect(ei,ej)
				if(length(int)>0){
					k <- k+1
				   new_edges[[k]] <- int
				}
			}
		}
		if(k==0) return(make_empty_hypergraph(horder(h)))
		new_edges <- unique(new_edges)
	   return(hypergraph_from_edgelist(new_edges))
	} else {
	   return(h)
	}
}

reduce.hypergraph.union <-
function(h)
{
	if(is.empty.hypergraph(h)) return(h)
	h <- remove.empty.hyperedges(h)
	edges <- hypergraph_as_edgelist(h)
	edges <- edges[order(edge_orders(h),decreasing=TRUE)]
	new_edges <- list(hnames(h))
	k <- 2
	while(length(edges)>0){
		eb <- edges[[1]]
		enb <- unique(c(unlist(edges[-1]),unlist(new_edges[-1])))
		if(length(setdiff(eb,enb)) != 0)
		{
			new_edges[[k]] <- eb
			k <- k+1
		}
		edges <- edges[-1]
	}
	h <- as.hypergraph(new_edges)
	delete.hyperedges(h,1)
}

xxxreduce.hypergraph.union <-
function(h)
{
	if(is.empty.hypergraph(h)) return(h)
	edges <- hypergraph_as_edgelist(h)
	edges <- edges[order(edge_orders(h),decreasing=TRUE)]
	new_edges <- list(hnames(h))
	k <- 2
	while(length(edges)>0){
		eb <- edges[[1]]
		enb <- unique(unlist(edges[-1]))
		if(length(setdiff(eb,enb)) != 0)
		{
			new_edges[[k]] <- eb
			k <- k+1
		}
		edges <- edges[-1]
	}
	h <- as.hypergraph(new_edges)
	delete.hyperedges(h,1)
}

simplify.hypergraph <- function(h)
{
   k <- remove.empty.hyperedges(remove.isolates(remove.loops(h)))
	edges <- unique(lapply(hyper_edges(k),sort))
	hypergraph_from_edgelist(edges)
}
