
#' Benchmark experiment graph.
#'
#' The benchmark summary plot takes the individual benchmark
#' experiment results into account. The y-axis represents the
#' data sets, the x-axis a podium with as many places as
#' candidate algorithms.
#'
#' @param x The object to plot
#' @export
bsgraph <- function(x, ...) {
  UseMethod('bsgraph')
}


#' @param x A \code{\link{dist}} object
#' @param ndists.show The number of distance levels to show
#' @param edge.col The color of edges (one or one for each distance level)
#' @param edge.lwd The line width of edges (one or one for each distance level)
#' @param node.fill The colors of nodes
#' @return The return value of \code{\link{bsgraph.graphNEL}}
#' @method bsgraph dist
#' @rdname bsgraph
bsgraph.dist <- function(x, ndists.show=length(sort(unique(x))),
                         edge.col=gray(0.7), edge.lwd=1,
                         node.fill=NULL, ...) {

  data <- as.matrix(x)
  
  nodes <- colnames(data)
  nnodes <- length(nodes)
  
  dists <- sort(unique(x))
  ndists <- length(dists)
  dshow <- dists[seq_len(ndists.show)]
  ndshow <- length(dshow)
  
  edge.col <- rep(edge.col, ndshow)
  edge.lwd <- rep(edge.lwd, ndshow)
  edge.len <- ceiling((1.2)^(seq_len(ndists)-1))
  edge.weight <- rev(seq_len(ndists))
  edge.lty <- c(rep('solid', ndshow),
                rep('blank', length(dists)-ndshow))

  graph <- new('graphNEL', nodes=nodes, edgemode='undirected')
  edgeAttrs <- list()
  nodeAttrs <- list()

  for ( i in 1:(nnodes-1) ) {
    for ( j in (i+1):nnodes ) {
      s <- data[i,j]

      if ( s %in% dshow ) {
        t <- which(s == dists)

        graph <- addEdge(nodes[i], nodes[j], graph, edge.weight[t])

        n <- paste(nodes[i], nodes[j], sep='~')
        edgeAttrs$len[n] <- edge.len[t]
        edgeAttrs$color[n] <- edge.col[t]
        edgeAttrs$lwd[n] <- edge.lwd[t]
        edgeAttrs$lty[n] <- edge.lty[t]
      }
    }
  }

  if ( !is.null(node.fill) )
    nodeAttrs$fillcolor[nodes] <- node.fill

  bsgraph(graph, nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs)
}


#' @param x A \code{\link[graph]{graphNEL} object
#' @param layoutType Defines the layout engine
#' @return Invisible return of the \code{\link[graph]{Ragraph}} object
#' @method bsgraph graphNEL
#' @rdname bsgraph
bsgraph.graphNEL <- function(x, layoutType='neato', ...) {
  
  agraph <- agopen(x, '', layoutType=layoutType, ...)
  plot(agraph)

  # Redraw nodes for beauty:
  par(new=TRUE)
  agraph2 <- agraph
  agraph2@AgEdge <- list()
  plot(agraph2)


  invisible(agraph)
}


