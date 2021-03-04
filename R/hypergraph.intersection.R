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

hypergraph.intersection <-
function(h1,h2,strict=FALSE)
{
   e1 <- hypergraph_as_edgelist(h1)
	if(is.null(e1)) return(h1)
   e2 <- hypergraph_as_edgelist(h2)
	if(is.null(e2)) return(h2)
	edges <- NULL
	if(strict){
		for(i in 1:length(e1)){
			z <- e1[[i]]
			k <- length(z)
			a <- any(unlist(lapply(e2,function(x) 
							length(x)==k && length(intersect(z,x))==k)))
			if(a){
				if(is.null(edges)){
					edges <- list(z)
				} else {
					edges <- c(edges,list(z))
				}
			}
		}
	} else {
		for(i in 1:length(e1)){
			z <- e1[[i]]
			a <- lapply(e2,function(x) intersect(z,x))
			edges <- c(edges,a)
		}
		l <- unlist(lapply(edges,length))
		if(all(l<2)) return(make_empty_hypergraph(0))
		if(any(l<2))
			edges <- edges[-which(l<2)]
	}
	if(is.null(edges)) return(make_empty_hypergraph(0))
	hypergraph_from_edgelist(unique(edges))
}
