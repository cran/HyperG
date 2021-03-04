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

# "borrowed" from igraph

igraphlayout <-
function (x, axes = FALSE, add = FALSE, xlim = c(-1, 1), ylim = c(-1, 
    1), mark.groups = list(), mark.shape = 1/2, mark.col = rainbow(length(mark.groups), 
    alpha = 0.3), mark.border = rainbow(length(mark.groups), 
    alpha = 1), mark.expand = 15, ...) 
{
    graph <- x
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    params <- from_igraph.parse.plot.params(graph, list(...))
    vertex.size <- 1/200 * params("vertex", "size")
    layout <- params("plot", "layout")
    rescale <- params("plot", "rescale")
    if (rescale) {
        layout <- norm_coords(layout, -1, 1, -1, 1)
    }
    invisible(layout)
}
