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

as.binary.hypergraph <- function(h)
{
   M <- incidence_matrix(h)
	M[M!=0] <- 1
	hypergraph_from_incidence_matrix(M)
}

equivalent.hypergraphs <- function(h1,h2,vertex.names=FALSE,edge.names=FALSE,
	 strip.names=FALSE,
    method=c('any','exact','binary'))
{
	m <- from_igraph.match.arg(method)
	if(is.null(m)) stop("invalid method")
	if(m != "any"){
		M1 <- incidence_matrix(h1)
		M2 <- incidence_matrix(h2)
		if(strip.names){
			if(!is.null(colnames(M1))){
				if(vertex.names){
					M1 <- M1[,order(colnames(M1))]
				}
				colnames(M1) <- 1:ncol(M1)
			}
			if(!is.null(colnames(M2))){
				if(vertex.names){
					M2 <- M2[,order(colnames(M2))]
				}
				colnames(M2) <- 1:ncol(M2)
			}
			if(!is.null(rownames(M1))){
				if(edge.names){
					M1 <- M1[order(rownames(M1)),]
				}
				rownames(M1) <- 1:nrow(M1)
			}
			if(!is.null(rownames(M2))){
				if(edge.names){
					M2 <- M2[order(rownames(M2)),]
				}
				rownames(M2) <- 1:nrow(M2)
			}
		} else {
			if(vertex.names){
				v1 <- colnames(M1)
				v2 <- colnames(M2)
				if(!all(sort(v1) == sort(v2))) return(FALSE)
				M1 <- M1[,order(v1)]
				M2 <- M2[,order(v2)]
			}
			if(edge.names){
				e1 <- rownames(M1)
				e2 <- rownames(M2)
				if(!all(sort(e1) == sort(e2))) return(FALSE)
				M1 <- M1[order(e1),]
				M2 <- M2[order(e2),]
			}
		}
		if(m != "exact"){
			h1 <- hypergraph_from_incidence_matrix(M1)
			h2 <- hypergraph_from_incidence_matrix(M1)
		}
	}
	switch(m,
	   exact=all(M1==M2),
		any=equivalent.hypergraphs(h1,h2,method='binary',strip.names=TRUE) ||
		    equivalent.hypergraphs(h1,h2,method='binary',strip.names=TRUE,
			           vertex.names=TRUE) ||
		    equivalent.hypergraphs(h1,h2,method='binary',strip.names=TRUE,
			           edge.names=TRUE) ||
		    equivalent.hypergraphs(h1,h2,method='binary',strip.names=TRUE,
						  vertex.names=TRUE,
			           edge.names=TRUE),
		binary=all(incidence_matrix(as.binary.hypergraph(h1))==
		           incidence_matrix(as.binary.hypergraph(h2)))
	)
}
