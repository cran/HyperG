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

plot.hypergraph <-
function(x,edge.color=NA,mark.groups=hypergraph_as_edgelist(h),
         layout,...)
{
	h <- x
	im <- incidence_matrix(h)
	z <- which(Matrix::rowSums(im)==0)
	if(length(z)>0){
		h <- hypergraph.delete.edges(h,z)
	}
	z <- which(Matrix::colSums(im)==0)
	if(length(z)>0){
	   a <- hnames(h)[z]
		h <- hypergraph.add.edges(h,as.list(a))
	}
	g <- hypergraph2graph(h)
	if(missing(layout)){
		if(!is.null(h$layout)){
		   layout <- h$layout
		} else {
			layout <- igraphlayout(g,mark.groups=mark.groups,...)
		}
	}
   plot(g,
		  layout=layout,
	     mark.groups=mark.groups,
		  edge.color=edge.color,
	     ...)
   invisible(layout) 
}
