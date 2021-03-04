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

remove.isolates <- function(h)
{
   x <- Matrix::colSums(incidence_matrix(h))
	isolated <- which(x==0)
	if(length(isolated)>0){
	   return(hypergraph.delete.vertices(h,isolated))
	}
	h
}

has.isolates <- function(h)
{
   x <- Matrix::colSums(incidence_matrix(h))
   any(x==0)
}

has.loops <- function(h)
{
   x <- Matrix::rowSums(incidence_matrix(h))
   any(x==1)
}

has.empty.hyperedges <- function(h)
{
   x <- Matrix::rowSums(incidence_matrix(h))
   any(x==0)
}

remove.loops <- function(h)
{
   x <- Matrix::rowSums(incidence_matrix(h))
	loops <- which(x==1)
	if(length(loops)>0){
	   return(hypergraph.delete.edges(h,loops))
	}
	h
}

remove.empty.hyperedges <- function(h)
{
   x <- Matrix::rowSums(incidence_matrix(h))
	empty <- which(x==0)
	if(length(empty)>0){
	   return(hypergraph.delete.edges(h,empty))
	}
	h
}

remove.duplicate.hyperedges <- function(h)
{
   edges <- lapply(hyper_edges(h),sort)
	hypergraph_from_edgelist(unique(edges))

}

