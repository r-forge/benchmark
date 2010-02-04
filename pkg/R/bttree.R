

### psychotree changes: ##############################################



btReg2 <- function(type = "loglin", ref = NULL, undecided = NULL, position = NULL) {
  new("StatModel",
    #capabilities = new("StatModelCapabilities"),
    capabilities = new(getClass('StatModelCapabilities', where = getNamespace('psychotree'))),
    name = "Bradley-Terry regression model",
    dpp = ModelEnvFormula,
    fit = function(object, weights = NULL, ...){

        save(object, weights, ref, undecided, position,
             file = 'btReg2-backup.RData')

        ## extract response (there are no regressors)
        y <- object@get("response")[[1]]

        #print('---fit---')
        #undecidable <- FALSE

        #if ( !is.null(weights) )
        #  if ( length(table(y[weights == 1])) < length(labels(y)) )
        #    undecidable <- TRUE

        ## call btReg.fit()
        #z <- withCallingHandlers(psychotree:::btReg.fit(y = y, weights = weights,
        #                                                type = type, ref = ref,
        #                                                undecided = undecided,
        #                                                position = position),
        #              warning = function(w) {print("warning"); undecidable <<- TRUE})
        tryCatch(
        z <- btReg.fit(y = y, weights = weights, type = type,
                       ref = ref, undecided = undecided, position = position),
                 error = function(e) save.image(file = sprintf('btReg-Error-%s.RData',
                                                as.integer(Sys.time()))))

	z$ModelEnv <- object
	z$addargs <- list(...)

        class(z) <- c('btReg2', class(z))

        #if ( undecidable )
        #  class(z) <- c('try-error', class(z))

        z
    }
  )
}

reweight.btReg2 <- function(object, weights, ...) {
    fit <- btReg2(type = object$type, ref = object$ref,
       undecided = object$undecided, position = object$position)@fit
     do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs))
}


bttree2 <- function(x, ...) {
  UseMethod('bttree2')
}

bttree2.bttreedata <- function(x, ...) {
  bttree2(preference ~ ., data = x, ...)
}

bttree2.default <- function(formula, data, na.action = na.pass,
                            type = "loglin", ref = NULL, undecided = NULL,
                            position = NULL, minsplit = 10, ...) {
  ## transform formula
  stopifnot(length(formula) > 2)
  formula <-  formula(terms(formula, data = data))
  ff <- y ~ 1 | x
  ff[[2]] <- formula[[2]]
  ff[[3]][[3]] <- formula[[3]]

  ## call mob()
  rval <- mob(ff, data = data,
              model = btReg2(type = type, ref = ref, undecided = undecided,
                             position = position),
              control = mob_control(minsplit = minsplit, ...), na.action = na.action)

  ## add class and return
  structure(list(mob = rval), class = c("bttree2", "bttree"))
}

######################################################################


as.bttreedata <- function(x, ...) {
  UseMethod('as.bttreedata')
}

as.bttreedata.bec <- function(x, ...) {
  stopifnot(!is.null(x$becp))
  stopifnot(!is.null(x$becc))

  stopifnot(all(dim(x$becp)[c(1, 4)] == 1))
  stopifnot(dim(x$becc)[1] == 1)

  ds <- as.data.frame(x$becc[1, ,])
  ds$preference <- as.paircomp(x$becp)

  structure(ds, class = c('bttreedata', class(ds)))
}

rbind.bttreedata <- function(...) {
  x <- rbind.data.frame(...)
  x$preference <- do.call(c, lapply(list(...), '[[', 'preference'))
  structure(x, class = c('bttreedata', class(x)))
}



