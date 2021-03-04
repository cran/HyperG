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

is.linear <- function(h)
{
   edges <- hypergraph_as_edgelist(h)
	s <- hsize(h)
	if(s != length(unique(edges))) return(FALSE)
	if(length(edges[[s]])==0) return(FALSE)
	for(i in 1:(s-1)){
	   ei <- edges[[i]]
		if(length(ei)==0) return(FALSE)
		for(j in (i+1):s){
		   ej <- edges[[j]]
			if(all(ei %in% ej) || all(ej %in% ei)) return(FALSE)
			if(length(intersect(ei,ej))>1) return(FALSE)
		}
	}
	TRUE
}

is.simple <- function(h)
{
	if(is.igraph(h)) return(igraph::is.simple(h))
   edges <- hypergraph_as_edgelist(h)
	s <- hsize(h)
	if(s != length(unique(edges))) return(FALSE)
	if(length(edges[[s]])==0) return(FALSE)
	for(i in 1:(s-1)){
	   ei <- edges[[i]]
		if(length(ei)==0) return(FALSE)
		for(j in (i+1):s){
		   ej <- edges[[j]]
			if(all(ei %in% ej) || all(ej %in% ei)) return(FALSE)
		}
	}
	TRUE
}

