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

hdegree <-
function(h)
{
	Matrix::colSums(h$M)
}

plotDegreeDistribution <- function(h,
											  xlab="Degree",
											  ylab="Density",
											  add.line=FALSE,
											  lty=2,lwd=1,line.col=1,
											  ...)
{
	if(is.igraph(h)) {
		deg <- degree(h)
	} else {
		deg <- hdegree(h)
	}
	hi <- hist(deg,-1:max(deg),plot=FALSE)$density
	z <- which(hi==0)
	X <- (1:length(hi))[-z]
	Y <- hi[-z]
	x <- log(X,10)
	y <- log(Y,10)
	plot(X,Y,xlab=xlab,ylab=ylab,log='xy',...)
	if(add.line){
	   l <- lm(y ~ x,data=data.frame(x=x,y=y))
		abline(reg=l,lty=lty,col=line.col,lwd=lwd)
	}
}
