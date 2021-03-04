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


hypergraph_from_literal <- function(...)
{
   mf <- as.list(match.call())[-1]
	
    f <- function(x) {
        if (is.call(x)) {
            if (length(x) == 3) {
                return(list(f(x[[2]]), op = as.character(x[[1]]), 
                  f(x[[3]])))
            }
            else {
                return(list(op = as.character(x[[1]]), f(x[[2]])))
            }
        }
        else {
            return(c(sym = as.character(x)))
        }
    }
    ret <- lapply(mf, function(x) setdiff(unlist(f(x)),"-"))
	 hypergraph_from_edgelist(ret)
}
