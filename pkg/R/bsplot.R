
bsplot <- function(x, ...) {
  UseMethod('bsplot')
}


bsplot.relation_ensemble <- function(x, perf=NULL, ...) {

  algs <- sort(as.character(relation_domain(x[[1]])[[1]]))

  ### Rank matrix:
  rankings <- lapply(tsort(x), as.ranking)
  rmatrix <- sapply(rankings, function(r)r[algs])

  
  ### Performance matrix:
  if ( is.null(perf) )
    pmatrix <- rmatrix
  else
    pmatrix <- perf
  

  ### Call "old" prototype code:
  .old.bsplot(rmatrix, performance=pmatrix, ...)
}


.old.bsplot <- function(ranking, performance, ylab=NULL,
                        col=NULL, perfcol=col,
                        success.rate=FALSE,
                        sig.lwd=3,
                        axis.lwd=1,
                        per.dataset=FALSE,
                        horizontal=FALSE,
                        xlas=2, ylas=2,
                        datasets.order=NULL, ...) {
  
  ndatasets = ncol(ranking)
  nalgorithms = nrow(ranking)
  
  datasets = colnames(ranking)
  algorithms = rownames(ranking)
  
  max.performance = {
    if ( per.dataset )
      apply(performance, 2, max)
    else
      rep(max(performance), ndatasets)
  }

  performance.order = apply(apply(performance, 2,
    function(x){sort(x, index.return=T, decreasing=success.rate)$ix}), 2,
    function(x){rownames(performance)[x]})

  if ( is.null(datasets.order) )
    datasets.order = do.call('order', lapply(1:nalgorithms,
      function(i) t(performance.order)[,i]))
  
            
  # Calculate plot attributes:
  rect.width = 1
  rect.height = 1
  space = 0
  irect.width = 0.6
  
  if ( !horizontal ) {  
    rects.ybottom = seq(rect.width, nalgorithms*rect.width, by=rect.width) - rect.width
    rects.xleft = seq(rect.height+space, ndatasets*(rect.height+space),
      by=(rect.height+space)) - (rect.height+space)
    xlim = c(min(rects.xleft), max(rects.xleft) + rect.width)
    ylim = c(min(rects.ybottom), max(rects.ybottom) + rect.height)
    
    myrect = rect

    dataset.axis = 1
    algorithm.axis = 2
    
    xlab = ''
    ylab = 'Podium'
  }
  else {
    rects.ybottom = seq(rect.height, nalgorithms*rect.height, by=rect.height) - rect.height
    rects.xleft = seq(rect.width+space, ndatasets*(rect.width+space),
      by=(rect.width+space)) - (rect.width+space)
    ylim = c(min(rects.xleft), max(rects.xleft) + rect.width)
    xlim = c(min(rects.ybottom), max(rects.ybottom) + rect.height)
    
    myrect = function(xleft, ybottom, xright, ytop, ...)
      rect(ybottom, xleft, ytop, xright, ...)

    dataset.axis = 2
    algorithm.axis = 1

    xlab = 'Podium'
    ylab = ''
  }

            
  # Plot it:
  plot.new()
  plot.window(xlim, ylim)
                      
  for ( j in 1:ndatasets ) {
    i = datasets.order[j]
    xleft = rep(rects.xleft[j], nalgorithms)
                            
    myrect(xleft,                               # xleft
           rects.ybottom,                       # ybottom
           xleft + rect.width,                  # xright
           rects.ybottom + rect.height,         # ytop
           col=col[performance.order[,i]],
           border=NA)

    myrect(xleft + (rect.width - irect.width) / 2,
           rects.ybottom,
           xleft + (rect.width - irect.width) / 2 + irect.width,
           rects.ybottom + performance[performance.order[,i],i] / max.performance[i],
           col=perfcol[performance.order[,i]],
           border=NA)
    
    sig = table(ranking[performance.order[,i],i])
    nsig = length(sig)
    sig.rects.ytop = cumsum(sig) * rect.height
    sig.rects.ybottom = c(0, sig.rects.ytop[-nsig])
    
    myrect(xleft[nsig],
           sig.rects.ybottom,
           xleft[nsig] + rect.width,
           sig.rects.ytop,
           lwd=sig.lwd, border=gray(0))
  }
            
  axis(dataset.axis, at=rects.xleft+(rect.width/2),
       tick=FALSE, labels=NA, lwd=axis.lwd)
  for ( i in 1:ndatasets )
    mtext(datasets[datasets.order[i]], side=dataset.axis,
          line=1, at=rects.xleft[i]+(rect.width/2), las=xlas)
                 
  axis(algorithm.axis, at=c(0, rects.ybottom+rect.height),
       labels=NA, lwd=axis.lwd)
  
  for ( i in 1:nalgorithms )
    mtext(paste(i, '.', sep=''), side=algorithm.axis, line=1,
          at=rects.ybottom[i]+(rect.height/2), las=ylas)
           
  box()
  title(xlab=xlab, ylab=ylab)
  
  invisible(perfcol)
}

