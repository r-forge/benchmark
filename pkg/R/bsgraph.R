

### "Old" prototype code: 
bsgraph <- function (dist,
                     layers=sort(unique(dist)),
                     nlayers=length(layers),
                     edgecol=rep(1, nlayers), edgelwd=rep(1, nlayers), upper.bound=+Inf,
                     nodefillcol=NULL, ...) {

  data = as.matrix(dist)
  nodes = colnames(data)

  layers = sort(unique(dist))
  layers = layers[layers < upper.bound]
  nlayers = length(layers)

  col = edgecol[1:nlayers]
  lwd = edgelwd[1:nlayers]
  len = ceiling((1.2)^(0:(nlayers-1)))
  weight = nlayers:1
  lty = c(rep('solid', sum(!is.na(col))), rep('blank', sum(is.na(col))))

  
  graph = new('graphNEL', nodes=nodes, edgemode='undirected')
  edgeAttrs = list()
  nodeAttrs = list()

  for ( i in 1:(length(nodes)-1) ) {
    for ( j in (i+1):length(nodes) ) {
      s = data[i,j]

      if ( s < upper.bound ) {
        t = which(s == layers)

        graph = addEdge(nodes[i], nodes[j], graph, weight[t])

        n = paste(nodes[i], nodes[j], sep='~')
        edgeAttrs$len[n] = len[t]
        edgeAttrs$color[n] = col[t]
        edgeAttrs$lwd[n] = lwd[t]
        edgeAttrs$lty[n] = lty[t]
      }
    }

    if ( !is.null(nodefillcol) )
      nodeAttrs$fillcolor[nodes[i]] = nodefillcol[i]
  }

  if ( !is.null(nodefillcol) )
    nodeAttrs$fillcolor[nodes[i+1]] = nodefillcol[i+1]
  

  agraph = agopen(graph, '', edgeAttrs=edgeAttrs, nodeAttrs=nodeAttrs, layoutType='neato')
  
  plot(agraph, ...)

  # Redraw nodes only for beauty:
  par(new=TRUE)
  agraph2 <- agraph
  agraph2@AgEdge <- list()
  plot(agraph2, ...)

  invisible(agraph)
}



