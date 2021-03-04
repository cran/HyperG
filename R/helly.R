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

## h <- hypergraph_from_edgelist(list(1:5,c(2,4,6,7),c(4:6,8,9),
##                                    9:10))
has.helly <- function(h, strong=FALSE)
{
	if(strong) return(strong.helly(h))
   n <- horder(h)
	if(n<=1) return(FALSE)
	M <- incidence_matrix(h)
	edges <- hyper_edges(h)
	for(a in 1:(n-1)){
		i <- hnames(h)[a]
	   for(b in (a+1):n){
			j <- hnames(h)[b]
			B <- M[,c(i,j)]
			Xxy <- which(Matrix::rowSums(B)==2)
			if(length(Xxy)>0){
				x <- unique(unlist(edges[which(M[,i]!=0)]))
				y <- unique(unlist(edges[which(M[,j]!=0)]))
				V <- union(x,y)
				for(v in V){
					if(v == i || v == j) break
					B <- M[,c(v,i)]
					Xxv <- which(Matrix::rowSums(B)==2)
					B <- M[,c(v,j)]
					Xyv <- which(Matrix::rowSums(B)==2)
					B <- M[union(Xxy,union(Xxv,Xyv)),,drop=FALSE]
					z <- Matrix::colSums(B)
					if(all(z<nrow(B))) return(FALSE)
				}
			}
		}
	}
	TRUE
}

generalized2section <- function(h)
{
   edges <- hyper_edges(h)
	do.call(rbind,lapply(1:hsize(h), function(i){
			edge <- edges[[i]]
	      pairs <- gtools::combinations(n=length(edge),v=edge,r=2)
			cbind(pairs,rep(i,nrow(pairs)))
	   }))
}

H2 <- function(h)
{
   line.graph(dual_hypergraph(h))
}

strong.helly <- function(h)
{
   g2s <- generalized2section(h)
	edgelist <- hyper_edges(h)
	n <- nrow(g2s)
	for(k in 1:n){
		x <- g2s[k,1]
		y <- g2s[k,2]
		e1 <- edgelist[[as.numeric(g2s[k,3])]]
		ex <- which(g2s[,1]==x)
		ey <- which(g2s[,2]==y)
		for(e2 in g2s[ex,]){
			z <- e2[3]
			ezi <- which((g2s[1,]) == x & (g2s[2,] == y))
			if(length(ezi) == 0) return(FALSE)
			for(e3 in g2s[ezi,]){
			   int <- intersect(intersect(e1,edgelist[[e2[3]]]),edgelist[[e3[3]]])
				if(!(x %in% int) || !(y %in% int) || !(z %in% int)) return(FALSE)
			}
		}
	}
	TRUE
}

is.helly <- function(h) {
	has.helly(h)
}
