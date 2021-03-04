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

sample_k_uniform_hypergraph <- function(n,m,k,prob)
{
	if(n<k) stop("n must be at least as large as k")
	if(missing(prob)){
	   prob <- rep(1/n,n)
	}
	M <- Matrix::Matrix(0,nrow=m,ncol=n)
	for(i in 1:m){
	   M[i,sample(n,k,replace=FALSE,prob=prob)] <- 1
	}
	hypergraph_from_incidence_matrix(M)
}

sample_k_regular_hypergraph <- function(n,m,k,prob)
{
	if(m<k) stop("m must be at least as large as k")
	if(missing(prob)){
	   prob <- rep(1/m,m)
	}
	M <- Matrix::Matrix(0,nrow=m,ncol=n)
	for(i in 1:n){
	   M[sample(m,k,replace=FALSE,prob=prob),i] <- 1
	}
	hypergraph_from_incidence_matrix(M)
}
