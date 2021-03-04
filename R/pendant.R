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

## h <- hypergraph_from_edgelist(list(1:4,2:5,c(4,6),c(7),c(3:5,8)))

pendant <- function(h)
{
	n <- horder(h)
	if(n==0) return(NULL)
	if(n==1) return(FALSE)
	M <- incidence_matrix(h)
	d <- hdegree(h)
   out <- rep(FALSE,n)
	twins <- vector('list',n)
	for(i in 1:(n-1)){
	   di <- d[i]
		if(di==0 && n>1){
		   out[i] <- TRUE
			twins[[i]] <- setdiff(1:n,i)
		} else {
			indi <- which(M[,i]==1)
			candidates <- setdiff(which(d>=di),i)
			if(length(candidates)>0){
				for(j in candidates){
					indj <- which(M[,j]==1)
					if(all(indi %in% indj)){
					   out[i] <- TRUE
						twins[[i]] <- c(twins[[i]],j)
					}
				}
			}
		}
	}
	v <- which(out)
	if(length(v)>0) twins <- twins[v]
	else twins <- NULL
	list(vertices=v,twins=twins)
}

