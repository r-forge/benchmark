

### psychotree changes: ##############################################

btReg2 <- function(type = "loglin", ref = NULL, undecided = NULL, position = NULL) {
  new("StatModel",
    #capabilities = new("StatModelCapabilities"),
    capabilities = new(getClass('StatModelCapabilities', where = getNamespace('psychotree'))),
    name = "Bradley-Terry regression model",
    dpp = ModelEnvFormula,
    fit = function(object, weights = NULL, ...){

        ## extract response (there are no regressors)
        y <- object@get("response")[[1]]

        print('---fit---')
        undecidable <- FALSE

        if ( !is.null(weights) )
          if ( length(table(y[weights == 1])) < length(labels(y)) )
            undecidable <- TRUE

        ## call btReg.fit()
        z <- psychotree:::btReg.fit(y = y, weights = weights, type = type, ref = ref,
                                    undecided = undecided, position = position)
	z$ModelEnv <- object
	z$addargs <- list(...)

        class(z) <- c('btReg2', class(z))

        if ( undecidable )
          class(z) <- c('try-error', class(z))

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


as.bttreedata <- function(...) {
  UseMethod('as.bttreedata')
}

as.bttreedata.psychobench <- function(...) {
  pc <- do.call(c, lapply(lapply(list(...), '[[', 'bench'), as.paircomp))
  ds <- do.call(rbind, lapply(list(...), '[[', 'dataset'))

  r <- as.data.frame(ds)
  r$preference <- pc

  structure(r, class = c('bttreedata', class(r)))
}


