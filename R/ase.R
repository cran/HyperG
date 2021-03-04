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

ase <-
function(g,verbose=FALSE,adjust.diag=FALSE,
					 laplacian=FALSE,
                normalize=FALSE,
					 scale.by.values=FALSE,
                vectors='u',d=2)
{
	if(d>gorder(g)) d <- gorder(g)
   if(verbose) cat("Extracting adjacency matrix\n")
	if(laplacian){
	   A <- laplacian_matrix(g,normalized=normalize)
	} else {
		A <- as_adjacency_matrix(g)
		if(adjust.diag){
			if(verbose) cat("Adjusting the diagonal of the adjacency matrix\n")
			n <- gorder(g)
			A <- A + Matrix::Diagonal(x=degree(g)/(n-1))
		}
		if(normalize){
			if(verbose) cat("Normalizing the adjacency matrix\n")
			d <- degree(g)
			dinv <- sqrt(1/d)
			dinv[d==0] <- 0
			Dinv <- Diagonal(x=dinv)
			A <- Dinv %*% A %*% Dinv
		}
	}
   if(verbose) cat("Computing the spectrum\n")
   if(!is.directed(g)){
      sv <- RSpectra::eigs(A,k=d)
      z <- sv$vectors
      if(scale.by.values) {
         ev <- sv$values
         ev[ev<=0] <- 1
         z <- scale(z,center=FALSE,scale=1/sqrt(ev))
      }
   } else {
      sv <- RSpectra::svds(A,k=d)
      if(tolower(vectors)=='u'){
         z <- sv$u
         if(scale.by.values) {
				sd <- sv$d
				sq <- sqrt(sd)
				sq[sd==0] <- 1
				z <- scale(z,center=FALSE,scale=1/sq)
			}
      } else if(tolower(vectors)=='v'){
         z <- sv$v
         if(scale.by.values){
				sd <- sv$d
				sq <- sqrt(sd)
				sq[sd==0] <- 1
				z <- scale(z,center=FALSE,scale=1/sq)
			}
      } else {
			u <- sv$u
			v <- sv$v
			if(scale.by.values){
				sd <- sv$d
				sq <- sqrt(sd)
				sq[sd==0] <- 1
				u <- scale(u,center=FALSE,scale=1/sq)
				v <- scale(v,center=FALSE,scale=1/sq)
			}
		   z <- cbind(u,v)
      }
   }
   z
}

lse <-
function(g,...)
{
   ase(g,adjust.diag=FALSE,laplacian=TRUE,...)
}
