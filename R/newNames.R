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

newNames <-
function(names,n)
{
	firsts <- unique(substring(names,1,1))
	v <- setdiff(c("V",letters,LETTERS),firsts)
	if(length(v)==0){
		x <- strsplit(names,split="")
		l <- c(letters,LETTERS)
		v <- ""
		for(i in 1:length(x)){
			a <- x[[i]]
			if(length(a)<i){
			   y <- "a"
			} else {
				y <- sample(setdiff(l,a[i]),1)
			}
			v <- paste(v,y,sep="")
			if(!(v %in% names)) break
		}
	} else {
	   v <- v[1]
	}
	paste0(v,1:n)
}
