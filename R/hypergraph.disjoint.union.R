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

hypergraph.disjoint.union <-
function(h1,h2)
{
	n1 <- hnames(h1)
	n2 <- hnames(h2)
	ni <- intersect(n1,n2)
	if(!is.null(ni)){
	   newn1 <- newNames(union(n1,n2),length(ni))
		ind <- match(ni,n1)
		colnames(h1$M)[ind] <- newn1
		n1 <- hnames(h1)
	   newn2 <- newNames(union(n1,n2),length(ni))
		ind <- match(ni,n2)
		colnames(h2$M)[ind] <- newn2
	}
   names <- c(hnames(h1),hnames(h2))
	M <- Matrix::Matrix(0,nrow=hsize(h1)+hsize(h2),
					   	  ncol=horder(h1)+horder(h2),
					   	  dimnames=list(NULL,names))
	M[1:hsize(h1),1:horder(h1)] <- h1$M
	M[(1:hsize(h2))+hsize(h1),(1:horder(h2))+horder(h1)] <- h2$M
   hypergraph_from_incidence_matrix(M)
}
