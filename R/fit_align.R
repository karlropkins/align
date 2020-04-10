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
#' @param lower The lowest values for \code{fun}, one per parameter.
#' @param upper The highest values for \code{fun}, one per parameter.
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
#basic alignment outputs sorted
#    but could all be tidied

##################################
#currently
# (KR) making alignment class
# front end handlers added
# by added/document
#################################
#to do
#various tidying
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
           fun = function(par, x) par[1] + (par[2]*x),
           lower = c(-60, -3), upper = c(60, 3),
           ...){
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
    #and cor messages
    #might need to kill warnings as well...
    log <- capture.output({
      ans <- RcppDE::DEoptim(function(par){
        x2 <- fun(par, x.index)
        ans <- approx(x2, y, x1, rule = 2)
        y <- ans$y
        ans <- 2-cor(x,y, use="pairwise.complete.obs")
        print(ans)
        ans <- if(is.na(ans)) 100 else ans
        ans <- ans * length(x1[!is.na(x1) & !is.na(y)])
        #sum((x-y)^2)    #only works because x,y same size
                        #NA handling needs thinking about
      }, lower, upper)
    })

    refs <- ans$optim$bestmem
#moved to print
#    cat(deparse(fun), "\nBest fit: ", refs)
    x2 <- fun(refs, x.index)

#return(ans)

    ##################################
    #this being kept to compare new/old
    #   outputs - can go once that is
    #   sorted
    ##################################
    ans2 <- approx(x2, y, x1, rule = 2)
    #this used to be rule 2...
    y <- ans2$y
    ans2 <-data.frame(x=x, y=y)

#replaces above currently testing
    ans3 <- warp_frame(d$y0, x2, x1)
    ans3 <- n_align(d$x0, ans3)

#not keeping ans2 once ans3 sorted
#rationalise ans, again might not keep all...
    alignment <- align_buildAlignment(method = "fit_align",
                                      ans = ans2, old.ans = ans3,
                                      sources = d,
                                      reports = ans,
                                      fun = fun)
    align_output(alignment, x.args$output)
  }



#####################################
#unexported functions
#####################################

#####################################
#alignment_fitAlignmentPlot
#####################################

#kr v0.0.1

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

