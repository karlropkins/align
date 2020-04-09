############################################
#' @title fit_align
############################################

#might change name...
#rebuild of previous sleeper.service function

#' @name fit_align
#' @description Time warp a data-series using a time-offsetting model.
#' @param x A first time-series \code{vector} or a \code{data.frame}
#' containing time-series to use a reference when warp-fitting
#' \code{y}.
#' @param y A second time-series \code{vector} or a \code{data.frame}
#' containing time-series to warp-fit with \code{x}.
#' @param by If \code{x} or \code{y} are \code{data.frames}, the names
#' of time-series to warp-fit.
#' @param fun A \code{function} describing the warp-fitting model to
#' be applied.
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

##################################
#currently
# (KR) making alignment class
# front end handlers added
# by added/document
#################################
#to do
# data.frame output handling
# object building functions
# print, plot, summary handling
# probably more
#      see notes in function
##################################

#' @rdname fit_align
#' @export
fit_align <-
  function(x, y = NULL, by = NULL, ...) {
    UseMethod("fit_align")
  }

#splatted function
#' @importFrom RcppDE DEoptim
#' @export
#' @method cow_align default
fit_align.default <-
  function(x, y=NULL, by=NULL,
           fun = function(par, x) par[1] + (par[2]*x), ...){
    #from sleeper.service
    #chopped front & end down, removed PEMS handling
    #tidy default settings


    x.args <- align_extraArgsHandler(...,
                                     default.method = "fit_align",
                                     default.output = c("summary", "plot", "ans"),
                                     ref.args = c("ans","plot", "offset",
                                                  "summary", "alignment"))

    #handle x, y, by...
    d <- align_XYByArgsHandler(x=x, y=y, by=by,
                               method = x.args$method)
    x <- d$x
    y <- d$y

    #set up
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
        sum((x-y)^2)    #only works because x,y same size
                        #NA handling needs thinking about
      }, c(-60, -3), c(60, 3))
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
