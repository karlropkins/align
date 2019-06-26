############################################
#' @title fitAlign
############################################

#might change name...
#rebuild of previous sleeper.service function

#' @name fitAlign
#' @description Time warp a data-series using a time-offsetting model.
#' @param x A first time-series \code{vector}, to use a reference when
#' warping \code{y}.
#' @param y A second time-series \code{vector}, to warp align with \code{x}.
#' @param fun A \code{function} describing the time-offsetting model to be
#' applied.
#' @param ... Other arguments, currently ignored.
#' @author Karl Ropkins


############################
#to do
############################
#lots - currently very crude
#documentation needs updating
#review third party methods - RcppDE might
#    not be best option...
#tidy fun handling and fun input handling,
#    so more flexible
#add common by and output handling
#think about print, plot, summary and
#    aligment outputs
#

#splatted function
#' @importFrom RcppDE DEoptim
#' @export
fitAlign <-
  function(x, y, fun = function(par, x) par[1] + (par[2]*x), ...){
    #from sleeper.service
    #chopped front & end down, removed PEMS handling
    #could replace this with align_XYByArgsHandler
    if(is.data.frame(x))
      x <- x[,1]
    if(is.data.frame(y))
      y <- y[,1]
    target.ln <- length(x)
    x1 <- 1:target.ln
    x.index <- 1:length(y)

    #messy jitter (needed?)
    #    (tested with) y <- y + (rnorm(length(y))*0.01)
    #tried optim, nls, nlm, GenSA,

    #using capture.output to hide DEoptim messages...
    log <- capture.output({
      ans <- RcppDE::DEoptim(function(par){
        x2 <- fun(par, x.index)
        ans <- approx(x2, y, x1, rule = 2)
        y <- ans$y
        sum((x-y)^2)    #only works because same size
      }, c(-60, -0.1), c(60, 3))
    })

    refs <- ans$optim$bestmem
    cat(deparse(fun), "\nBest fit: ", refs)
    x2 <- fun(refs, x.index)
    ##################################
    #this is repeated code
    #   think about warp_frame option
    #   here and previously
    ##################################
    ans <- approx(x2, y, x1, rule = 2)
    y <- ans$y
    data.frame(x=x, y=y)
  }
