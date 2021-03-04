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

is.star <- function(h,type=c("weak","strong"))
{
	m <- from_igraph.match.arg(type)
   edges <- hypergraph_as_edgelist(h)
	s <- hsize(h)
	if(s==0) return(FALSE)
	if(s==1) {
		if(length(edges[[1]])==0) return(FALSE)
		return(TRUE)
	}
	inter <- edges[[1]]
	for(i in 2:s){
      inter <- intersect(inter, edges[[i]])
	}
	u <- NULL
	si <- length(inter)
	if(si == 0) return(FALSE)
	if(m=="weak") return(TRUE)
	for(i in 1:s){
	   x <- setdiff(edges[[i]],inter)
		if(m=="strong"){
			if(length(x)==0) return(FALSE)
		}
	   u <- c(u,x)
	}
	horder(h) == (length(inter)+length(u))
}


intersection_set <- function(h)
{
   edges <- hypergraph_as_edgelist(h)
	s <- hsize(h)
	if(s==0) return(NULL)
	if(s==1) {
		return(edges[[1]])
	}
	inter <- edges[[1]]
	for(i in 2:s){
      inter <- intersect(inter, edges[[i]])
	}
	if(length(inter)==0) return(NULL)
	return(inter)
}

