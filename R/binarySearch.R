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

remove.redundant.vertices <- function(H, 
								check.empty=TRUE
								)
{
	 if(!hypergraph.is.connected(H)) stop("H is disconnected")
	 f <- function(guess,h,v,check.empty){
		 inds <- v[(1:guess)]
		 myHype <- hypergraph.delete.vertices(h, inds)
		 if(check.empty && has.empty.hyperedges(myHype)) return(-1)
		 if(hypergraph.is.connected(myHype)) 
		 {
			  if(guess>1){
				  inds <- inds[-length(inds)]
				  myHype <- hypergraph.delete.vertices(h,inds)
				  if(check.empty && has.empty.hyperedges(myHype)){
				     return(1)
				  }
				  if(!hypergraph.is.connected(myHype)){
					  return(0)
				  }
			  }
			  return(1)
		 } 
		 else 
		 {
			  return(-1)
		 }
    }
	 vorder <- order(hdegree(H),decreasing=TRUE)
	 suppressWarnings(
	 z <- gtools::binsearch(f,c(1,horder(H)),h=H,
									v=vorder,
									check.empty=check.empty)
	 )
	 h1 <- hypergraph.delete.vertices(H,vorder[1:z$where[1]])
	 if(length(z$where)==2){
	    h2 <- hypergraph.delete.vertices(H,vorder[1:z$where[2]])
		 if(!hypergraph.is.connected(h1) || 
		    (check.empty && has.empty.hyperedges(h1))){
		    h1 <- h2
		 } else if(horder(h2)<horder(h1)){
		    if(hypergraph.is.connected(h2) && 
			    !(check.empty && has.empty.hyperedges(h2))){
			    h1 <- h2
			 }
		 }
	 } else if((z$flag=="Upper Boundary") && (z$value==(-1))){
	    h1 <- H
	 }
	 h1
}

