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

sample_geom_hypergraph <-
function(n,m,d=2,X,Y,radius,method="Euclidean",
thresh.method='leq', uniformly=FALSE)
{
	if(missing(X)){
	 if(missing(n)) stop("n or X, must be provided")
	 X <- matrix(runif(2*n),ncol=d)
	}
	if(missing(Y)){
	 if(missing(m)) stop("m or Y, must be provided")
	 Y <- matrix(runif(2*m),ncol=d)
	}
	D <- proxy::dist(Y,X,method=method)
	if(missing(radius)){
		if(uniformly){
		   radius <- runif(1,min(D),max(D))
		} else {
			radius <- sample(unique(as.vector(D)),1)
		}
	}
	D <- as.matrix(D)
	if(thresh.method=='geq'){
		M <- Matrix::Matrix(as.numeric(D>=radius),ncol=ncol(D),nrow=nrow(D))
	} else {
		M <- Matrix::Matrix(as.numeric(D<=radius),ncol=ncol(D),nrow=nrow(D))
	}
	h <- hypergraph_from_incidence_matrix(M)
	h$Y <- Y
	h$X <- X
	h$radius <- radius
	h
}

plot_geom_hypergraph <- function(h,pch=20,cex=3,col='gray',
											plotY=TRUE,plot.circles=plotY,
											full.circles=TRUE,
											lty=2,lcol='black',
                                 ...)
{
	X <- h$X
	Y <- h$Y
	R <- h$radius
	if(plotY){
		xlim <- range(c(X[,1],Y[,1]))
		ylim <- range(c(X[,2],Y[,2]))
	} else {
		xlim <- range(X[,1])
		ylim <- range(X[,2])
	}
	if(plot.circles){
		n <- nrow(Y)
		if(length(lcol)<n) lcol <- rep(lcol,n)
		theta <- seq(0,2*pi,length=1000)
		z <- cbind(R*cos(theta),R*sin(theta))
		if(full.circles){
			for(i in 1:n){
				xlim[1] <- min(xlim[1],z[,1]+Y[i,1])
				xlim[2] <- max(xlim[2],z[,1]+Y[i,1])
				ylim[1] <- min(ylim[1],z[,2]+Y[i,2])
				ylim[2] <- max(ylim[2],z[,2]+Y[i,2])
			}
		}
	}
   plot(h,layout=X,rescale=FALSE,xlim=xlim,ylim=ylim,...)
	if(plotY){
		points(h$Y,pch=pch,col=col,cex=cex)
	}
	if(plot.circles){
		for(i in 1:n){
			lines(z[,1]+Y[i,1],z[,2]+Y[i,2],lty=lty,col=lcol[i])
		}
	}
}
