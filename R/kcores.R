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

# From hypergraph, a Bioconductor package.
# https://git.bioconductor.org/packages/hypergraph
#   
# Reference:
#   A hypergraph model for the yeast protein complex network
# By E. Ramadan, A. Tarafdar, A. Pothen
#   Procs. Workshop High Performance Computational Biology, IEEE/ACM 2004
#   
# algorithm for computing the k-core of a hypergraph:
# ===================================================
#   
#   while there are vertices with degree < k do
#   {
#      for each such vertex v do
#      {
#         for each hyperedge f associated with v do
#         {
#            delet v from adj(f)
#            decrement d(f) by 1
#   	     if f is non-maximal then
#            {
#   	        for each vertex w associated with f do
#   	        {
#                  delete f from adj(w)
#                  decrement d(w) by 1
#                  if ( d(w) < k then
#                  {
#                     include w in list of vertices with degree < k
#   	           }
#               }
#            }
#         }
#      }
#   }
#   
#h <- hypergraph_from_edgelist(list(1:4,
#   letters[3:6],
#	c(2,7:10),
#	c(letters[c(3,6)],c(10,12)),
#	c(11,13:15),
#	letters[c(11,13:15)],
#	c(1,letters[4:6],10:12,letters[13],15,16),
#	c(1,4:6,10:12,letters[13],13,letters[14],14,16)))
#
#h <- hypergraph_from_edgelist(list(c("A", "C"), c("B", "C"), c("C", "E"), c("C", "F"), c("E", "D"), c("E", "F"), c("D", "G"), c("D", "H"), c("D", "J"), c("H", "G"), c("H", "J"), c("G", "J"), c("J", "M"), c("J", "K"), c("M", "K"), c("M", "O"), c("M", "N"), c("K", "N"), c("K", "F"), c("K", "I"), c("K", "L"), c("F", "I"), c("I", "L"), c("F", "L"), c("P", "Q"), c("Q", "R"), c("Q", "S"), c("R", "T"), c("S", "T"),"U"))

kCores <- function(h)
{
   nv <- horder(h)
	ne <- hsize(h)

	im <- t(as.matrix(incidence_matrix(h)))
	rownames(im) <- hnames(h)
	colnames(im) <- 1:ncol(im)
	v_deg <- sort(hdegree(h))

   core <- array(0, nv, dimnames = list(hnames(h)))

	k_num <- 0
	for ( i in 1:nv )
	{
		v <- names(v_deg)[i]
		k_num <- max(v_deg[v], k_num)
		core[v] <- k_num

		# v's hyperedges
		he_set <- which(im[v,] == 1)
		im[v, he_set] <- 0

		# remove non-maximal hyperedges
		# (1) selective approach
		for ( f in names(he_set) )
		{
			# hyperedges adjacent to f
			r_chosen <- which(im[, f] == 1)
			x <- apply(im[r_chosen,,drop=FALSE],2,sum)
			c_chosen <- which(x > 0)
			im_sub <- im[r_chosen, c_chosen,drop=FALSE] 

			for ( g in names(c_chosen) )
				if ( f != g && im_sub[, f] == im_sub[, g] )
				{
					im[, f] <- 0
				}
		}
		v_deg <- sort(rowSums(im))
	}
	core
}
