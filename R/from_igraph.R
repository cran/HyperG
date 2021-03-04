## Non-exported functions borrowed from the igraph package, and
## slightly modified (or not) to make my life easier.


#   IGraph R package
#   Copyright (C) 2003-2012  Gabor Csardi <csardi.gabor@gmail.com>
#   334 Harvard street, Cambridge, MA 02139 USA
#   
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc.,  51 Franklin Street, Fifth Floor, Boston, MA
#   02110-1301 USA
#
###################################################################

###################################################################
# Common functions for plot and tkplot
###################################################################

from_igraph.parse.plot.params <- function(graph, params) {
  
  ## store the arguments
  p <- list(vertex=list(), edge=list(), plot=list())
  for (n in names(params)) {
    if (substr(n, 1, 7)=="vertex.") {
      nn <- substring(n, 8)
      p[["vertex"]][[nn]] <- params[[n]]
    } else if (substr(n, 1, 5)=="edge.") {
      nn <- substring(n, 6)
      p[["edge"]][[nn]] <- params[[n]]
    } else {
      p[["plot"]][[n]] <- params[[n]]
    }
  }
  
  func <- function(type, name, range=NULL, dontcall=FALSE) {
    if (! type %in% names(p)) {
      stop("Invalid plot option type")
    }
    ret <- function() {
      v <- p[[type]][[name]]
      if (is.function(v) && !dontcall) {
        v <- v(graph)
      }
      if (is.null(range)) {
        return (v)        
      } else {
        if (length(v)==1) {
          return(rep(v, length(range)))
        } else {
          return (rep(v, length=max(range)+1)[[range+1]])
        }
      }
    }
    if (name %in% names(p[[type]])) {
      ## we already have the parameter
      return(ret())
    } else {
      ## we don't have the parameter, check attributes first
      if (type=="vertex" && name %in% vertex_attr_names(graph)) {
        p[[type]][[name]] <- vertex_attr(graph, name)
        return(ret())
      } else if (type=="edge" && name %in% edge_attr_names(graph)) {
        p[[type]][[name]] <- edge_attr(graph, name)
        return(ret())
      } else if (type=="plot" && name %in% graph_attr_names(graph)) {
        p[[type]][[name]] <- graph_attr(graph, name)
        return(ret())
      } else {
        ## no attributes either, check igraph parameters
        n <- paste(sep="", type, ".", name)
        v <- igraph_opt(n)
        if (!is.null(v)) {
          p[[type]][[name]] <- v
          return(ret())
        }
        ## no igraph parameter either, use default value
        p[[type]][[name]] <- igraph.i.default.values[[type]][[name]]
        return(ret())
      }
    }
    
  }

  return (func)
}

igraph_vertex.default <- list(color=1,
                         size=15,
                         size2=15,
                         label=NA,
                         label.degree=-pi/4,
                         label.color="darkblue",
                         label.dist=0,
                         label.family="serif",
                         label.font=1,
                         label.cex=1,
                         frame.color="black",
                         shape="circle",
                         pie=1,
                         pie.color=list(c("white", "lightblue", "mistyrose",
                           "lightcyan", "lavender", "cornsilk")),
                         pie.border=list(c("white", "lightblue","mistyrose",
                           "lightcyan", "lavender", "cornsilk")),
                         pie.angle=45,
                         pie.density=-1,
                         pie.lty=1,
                         raster=NA)

igraph_edge.default <- list(color="darkgrey",
                       label=NA,
                       lty=1,
                       width=1,
                       loop.angle=0,
                       loop.angle2=0,
                       label.family="serif",
                       label.font=1,
                       label.cex=1,
                       label.color="darkblue",
                       label.x=NULL,
                       label.y=NULL,
                       arrow.size=1,
                       arrow.mode=NA,
                       curved=NA,
                       arrow.width=1)

igraph_plot.default <- list(palette=categorical_pal(8),
                       layout=layout_nicely,
                       margin=c(0,0,0,0),
                       rescale=TRUE,
                       asp=1,
                       frame=FALSE,
                       main=NA,
                       sub="",
                       xlab=NA,
                       ylab="")

igraph.i.default.values <- new.env()

igraph.i.default.values[["vertex"]] <- igraph_vertex.default
igraph.i.default.values[["edge"]]   <- igraph_edge.default
igraph.i.default.values[["plot"]]   <- igraph_plot.default

## Borrowed from igraph

from_igraph.match.arg <- function(arg, choices, several.ok=FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  m <- try(match.arg(arg=arg, choices=choices, several.ok=several.ok),
           silent=TRUE)
  if(inherits(m,'try-error')) return(NULL)
  m
}

