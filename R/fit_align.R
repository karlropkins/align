############################################
#' @title fit_align
############################################

#might change name...
#rebuild of previous sleeper.service function

#' @name fit_align
#' @aliases fit_align
#' @description Time warp a data-series using a time-offsetting model.
#' @param x A first time-series \code{vector} or a \code{data.frame}
#' containing time-series to use a reference when warp-fitting
#' \code{y}.
#' @param y A second time-series \code{vector} or a \code{data.frame}
#' containing time-series to warp-fit with \code{x}.
#' @param by If \code{x} or \code{y} are \code{data.frames}, the names
#' of time-series to warp-fit.
#' @param fun A \code{function} describing the warp-fitting model
#' to be applied.
#' @param lower The lowest values for \code{fun}, one per parameter.
#' @param upper The highest values for \code{fun}, one per parameter.
#' @param ... Other arguments currently include:
#' \describe{
#'   \item{\code{output}}{The default \code{..._align} \code{output}
#'   is \code{c("summary", "plot", "ans")}. \code{output} options
#'    include: \code{"ans"}, \code{"plot"}, \code{"summary"} and
#'    \code{"alignment"}. Multiple \code{output}s are allowed, but
#'    only the last is captured by return. \code{alignment} is a
#'    special object class which may be helpful to those looking at
#'    alignments in more detail.}
#' }
#' @author Karl Ropkins


############################
#to do
############################
#lots - currently very crude
#documentation needs updating
#review third party methods - RcppDE might
#    not be best option...
#    tried optim, nls, nlm, GenSA...
#tidy fun handling and fun input handling,
#    so more flexible
#    pass control... to RcppDE::DEoptim
#    same re optimisation function...
#check fun handling in RcppDE::DEoptim
#    worked but looks odd...
#basic alignment outputs sorted
#    but could all be tidied

##################################
#currently
# (KR) recoding weighting function
#################################
#to do
#various tidying
#      see notes in function
##################################

#' @rdname fit_align
#' @export
fit_align <-
  function(x, y = NULL, by = NULL, fun, lower, upper, ...) {
    UseMethod("fit_align")
  }

#splatted function
## #' @rdname fit_align
#' @importFrom RcppDE DEoptim
#' @method fit_align default
#' @export
fit_align.default <-
  function(x, y=NULL, by=NULL,
           fun = function(par, x) par[1] + (par[2]*x),
           lower = c(-60, 0.3), upper = c(60, 3),
           ...){
    #from sleeper.service
    #chopped front & end down, removed PEMS handling

    #using align_extraArgsHandler to handle x.args
    x.args <- align_extraArgsHandler(...,
                                     default.args = list(method = "fit_align",
                                                         output = c("summary", "plot", "ans")),
                                     ref.args = c("ans","plot", "offset",
                                                  "summary", "alignment"))

    #handle x, y, by...
    d <- align_XYByArgsHandler(x=x, y=y, by=by,
                               method = x.args$method)
    x <- d$x
    y <- d$y

    #set up
    x1 <- 1:length(x)
    x.index <- 1:length(y)

    #jitter needed for perfect cases?
    #    (tested with) y <- y + (rnorm(length(y))*0.01)

    #using capture.output to hide DEoptim messages...
    #might need to kill warnings as well...
    log <- capture.output({
      ans <- RcppDE::DEoptim(function(par){
        x2 <- fun(par, x.index)
        ans <- approx(x2, y, x1, rule = 1)
        y <- ans$y
########################
#replace next bit with a
#arg supplied function
########################
        ans <- 2-cor(x,y, use="pairwise.complete.obs")
        ans <- if(is.na(ans)) Inf else ans
#        ans <- sum((x-y)^2, na.rm=TRUE)
#above nice if you are chasing x=y
#print(length(x1[!is.na(x) & !is.na(y)]))
        if(length(x1[!is.na(x) & !is.na(y)])<50)
           ans <- Inf

#        ans <- ans / length(x1[!is.na(x) & !is.na(y)])
#        if(is.na(ans)) return(100*length(x))
#        ans <- ans / length(x[!is.na(x) & !is.na(y)])
#        if(is.na(ans)) return(100*length(x))
        ans
#need to find better optimisation function...
#x,y need to be same size for second version
#    look at discussion in ?RcppDE::DEoptim
#also NA handling needs thinking about
      }, lower, upper, control=list(F=0.1))
    })

    refs <- ans$optim$bestmem

    x2 <- fun(refs, x.index)
    temp <- range(c(x1,x2), na.rm=TRUE)
    temp <- floor(temp[1]):ceiling(temp[2])

    ##################################
    #this being kept to compare new/old
    #   outputs - can go once that is
    #   sorted
    ##################################
    ans2 <- approx(x2, y, temp, rule = 1)
    y <- ans2$y
    ans2 <-n_align(x=x, y=y, n=min(temp, na.rm=TRUE)-1)

    #ans2 at moment work but only for vectors

    #replaces above currently testing
    #    ans3 <- warp_frame(d$y0, x2, x1)
    ans3 <- warp_frame(d$y0, x2, temp)
    ans3 <- n_align(d$x0, ans3, n=min(temp, na.rm=TRUE)-1)

    #not keeping ans2 once ans3 sorted
    #rationalise ans, again might not keep all...
    alignment <- align_buildAlignment(method = "fit_align",
                                      ans = ans3, old.ans = ans2,
                                      sources = d,
                                      reports = ans,
                                      fun = fun)

    #output as alignment
    align_output(alignment, x.args$output)
  }



#####################################
#unexported functions
#####################################

#####################################
#alignment_fitAlignmentPlot
#####################################

#kr v0.0.1

#####################################
#to do
#####################################
#provisional plot
#   think about something better...

#' @import ggplot2
alignment_fitAlignmentPlot <-
  function(x, ...){
    df <- data.frame(iteration = 1:length(x$reports$member$bestvalit),
                     scores = x$reports$member$bestvalit)
    ggplot(df, aes(x=iteration, y=scores)) +
      #geom_point() +
      geom_path(aes(x=iteration, y=scores)) +
      ylab("Warp (Fit) Score") +
      xlab("Fit Iteration") +
      theme_bw()
  }

