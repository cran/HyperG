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

knn_hypergraph <- function(x,k=1,method="Euclidean",reduce=FALSE,
   as.graph=FALSE)
{
	if(is.vector(x)) x <- matrix(x,ncol=1)
	n <- nrow(x)
	if(length(k)<n) k <- rep(k,n)
	k <- k+1  ## to account for the vertex itself
   M <- matrix(0,nrow=n,ncol=n)
	D <- as.matrix(proxy::dist(x,method=method))
	for(i in 1:n){
		ord <- order(D[i,])
		M[i,ord[1:k[i]]] <- 1
	}
	if(as.graph) return(graph_from_adjacency_matrix(M,mode='directed'))
	h <- hypergraph_from_incidence_matrix(M)
	if(reduce) h <- reduce.hypergraph(h,method="inclusion")
	h
}
