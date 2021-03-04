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

sample_sbm_hypergraph <-
function(n,P,block.sizes,d,impurity=0,variable.size=FALSE,
   absolute.purity=TRUE)
{
	k <- nrow(P)

	if(missing(n)) n <- sum(block.sizes)

	tau <- rep(1:k,times=block.sizes)

	g <- sample_sbm(n,P,block.sizes)
	m <- gsize(g)

	M <- incidence_matrix(g)

	if(variable.size){
		s <- rpois(m,d)+2
	} else {
		s <- rep(d,m)
	}
	for(kk in 1:m){
		if(s[[kk]]>2){
			z <- which(M[kk,]==1)
			i <- z[1]
			j <- z[2]
			indsij <- setdiff(which(tau %in% tau[z]),z)
			samp <- sample(indsij,s[kk]-2)
			M[kk,samp] <- 1
			if(impurity>0){
				if(k==2 && (z[1]!=z[2])){
				   next
				}
				ni <- min(length(samp),impurity)
				x <- sample(samp,ni)
				M[kk,x] <- 0
				if(absolute.purity){
					indsnotij <- which(!(tau %in% tau[z]))
				} else {
					indsnotij <- setdiff(1:n,union(x,which(M[kk,]==1)))
				}
				M[kk,sample(indsnotij,length(x))] <- 1
			}
		}
	}
	hypergraph_from_incidence_matrix(M)
}

