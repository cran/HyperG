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


hypergraph_laplacian_matrix <- function(h,normalize=FALSE)
{
	
	if(is.empty.hypergraph(h)) {
		return(Matrix::Matrix(0,nrow=hsize(h),ncol=horder(h)))
	}
   A <- hypergraph_as_adjacency_matrix(h)
	d <- Matrix::rowSums(A)
	if(any(d==0)) {
	   j <- which(d==0)
		A <- A[-j,-j]
		d <- d[-j]
		warning("isolated vertex removed")
	}
	D <- Matrix::Diagonal(x=d)
	L <- D-A
	if(normalize){
		D2 <- Matrix::Diagonal(x=1/sqrt(d))
		L <- D2 %*% L %*% D2
	}
	L
}


