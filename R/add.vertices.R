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

hypergraph.add.vertices <-
function(h,nv,names)
{
	if(missing(names)){
		cols <- newNames(hnames(h),nv)
	} else if(length(names)==1 && is.na(names)){
	   cols <- as.character((1:nv)+horder(h))
	} else if(length(names)<nv){
	    cols <- c(names,
		           newNames(hnames(h),nv-length(names)))
	} else {
	    cols <- as.character(names)
	}
	M <- Matrix::Matrix(0,nrow=hsize(h),ncol=nv,dimnames=list(NULL,cols))
	M <- cbind(incidence_matrix(h),M)
	hypergraph_from_incidence_matrix(M)
}

