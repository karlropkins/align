############################################
#' @title cor_align
############################################

#' @name cor_align
#' @description Correlation based (linear) alignment, row
#' offsetting second of two \code{vector}s or \code{data.frame}s
#' based on best fit as estimated by correlation coefficient.
#' @param x First \code{vector} or \code{data.frame}, to be aligned
#' and merged with.
#' @param y Second \code{vector} or \code{data.frame}, to align and
#' merge with \code{x}.
#' @param by If \code{x} or \code{y} are \code{data.frame}s, the names
#' of the columns that \code{cor_align} should align with.
#' @param ... Other arguments:
#' \describe{
#'    \item{\code{min.overlap}}{By default, \code{cor_align} compares
#'    correlations for lags with \code{x}/\code{y} overlaps of
#'    2000 rows or 20\% if smaller. Other minimum (row number)
#'    overlaps can be set using the extra argument
#'    \code{min.overlap}.}
#'    \item{\code{output}}{The default \code{cor_align} \code{output}
#'    is \code{c("plot", "summary", "ans")}. \code{output} options
#'    include: \code{"ans"}, \code{"plot"}, \code{"summary"} and
#'    \code{"alignment"}. Multiple \code{output}s are allowed, but
#'    only the last is captured by return. \code{alignment} is a
#'    special object class which may be helpful to those looking at
#'    alignments in more detail.}
#' }
#' @author Karl Ropkins
#' @return By default, \code{cor_align} returns \code{x} and \code{y}
#' as an aligned \code{data.frame}. It also provides a lag correlation
#' profile (plot) and alignment report. See above about modifying
#' outputs.
#' @note \code{cor_align} is based on \code{cAlign} function in
#' earlier version of \code{pems.utils}. It works with vectors,
#' data.frames and other objects that can be converted to data.frame
#' with as.data.frame(), and uses \code{Rcpp} to run \code{C++} code.

#splatted function
#(from pems.utils)
#kr 15/11/2018 v 0.5.0
#that was version update from RDE work...

###############################
#TO DO
###############################
#NOTE: Possible issue...
#look at how cor_align offset calculated
#not a whole number when (nrows) large
#   and min.overlap small n(rows) * 0.999
#   in IPSOS work
######################################
#NOTE: possible issue...
#require(align, dplyr)
#cor_align(iris, iris) %>%
#  group_by(Species.x) %>%
#  summarise(count =n())
##(error message object spacing messy)
######################################
#not completely happy with cor_align
#    method


#' @rdname cor_align
#' @export
cor_align <-
function(x, y = NULL, by = NULL, ...) {
  UseMethod("cor_align")
}


#' @export
#' @method cor_align default
cor_align.default <-
function(x, y = NULL, by = NULL, ...){

    #linear offset alignment using best correlation
    #to estimate n, the nAlign...

    #NOTE:
    #This is based on previous pems.utils::cAlign
    #(C_ylagxCORR was written for pems.utils)

    #structure here
    #as documented above
    #NOTE: this is simplification
    #      dropped pems class handling and formula structure

    #to do
    #######################
    #tidy error messages
    #input handling

    #handle stuff currently not declared in formals
    #       also setup for general output management...
    #align_extraArgsHandler in unexported code
    x.args <- align_extraArgsHandler(...,
                  default.method = "cor_align",
                  default.output = c("summary", "plot", "ans"),
                  ref.args = c("ans","plot", "offset",
                               "summary", "alignment"))

    #x, y and by general output handler
    #align_XYByArgsHandler in unexported code
    ###########################
    #note
    ###########################
    #this currently limits
    #cor_align to use with data.frames
    #and valid as.data.frame()s
    ###########################
    #to do
    ###########################
    #handle missing x?
    #return aligned.y?
    d <- align_XYByArgsHandler(x=x, y=y, by=by,
                               method = x.args$method)

    #x and y cases to be used for alignment
    x <- d$x
    y <- d$y

    #############################
    #main routine
    #############################
    #this is faster than version 0.3, 0.4 but
    #this is still slower than older cAlign (0.1, 0.2) that
    #   is used stat ccf function BUT did not always find
    #   best fit if toward edge, roughly
    #   offset > 0.75 * length(y)
    #this gives consistently as good or better fit than
    #   0.1, 0.2, so not ideal but good trade-off...

    #fit smallest to biggest
    if(length(y)>length(x)){
      temp <- y
      y <- x
      x <- temp
      reversed=TRUE
    } else {
      reversed=FALSE
    }

    #set min.overlap if not in call
    if(!"min.overlap" %in% names(x.args))
      x.args$min.overlap <- min(c(floor(min(length(x),
                                length(y))*0.2), 2000))
    pad <- length(x) - x.args$min.overlap
    y <- c(rep(NA, pad), y, rep(NA, pad))

    #use C_ylagxCOR to find best fit alignment
    lag.scs <- .Call("_align_C_ylagxCOR", x, y)
    index <- (1:length(lag.scs)) - length(x) + x.args$min.overlap - 1
    if(!reversed) index <- -index
    bst.lag <- index[which(lag.scs==max(lag.scs, na.rm=TRUE))[1]]
                                    #NOTE: [1] in case tie!!

    #######################################################
    #NOTE:
    #from here this is compremise ()
    #we build the full alignment object and pull out the
    #    bits we want rather than just building the bits we
    #    want when we want them...
    #######################################################

    #use nAlign to merge full datasets
    ans <- n_align(d$x0, d$y0, n = bst.lag, output = "ans")

    #make alignment object
    #    align_buildAlignment in unexported code
    #    (if objects do not get anymore complicated
    #        should probably drop function and do directly)
    alignment <- align_buildAlignment(method = "cor_align",
                                      ans = ans, sources = d,
                                      reports = list(index = index,
                                                    scores = lag.scs,
                                                    offset = bst.lag),
                                      offset = bst.lag)

    #output
    #align_output in unexported code
    #this plus align_extraArgsHandler and align_XYByArgsHandler
    #handle common elements of the ..._align functions
    #     outputs = ans, plot, summary alignment object, etc.
    align_output(alignment, x.args$output)
}


#unexported functions

##################################
#alignment_corAlignPlot
##################################

#kr v.0.0.3
#update of previous
#plot, then lattice::xyplot, now ggplot2::ggplot
#  not sure what works best here
############################
#to do
############################
#code needs tidying to allow user modification
#

#' @import ggplot2
alignment_corAlignmentPlot <-
  function(x, ...){
    #basic plot
#this needs some argument passing?
#blue lines? if so, make other plots similar..?
    df <- data.frame(index = x$reports$index,
                     scores = x$reports$scores)
    plt <- ggplot(df) +
      geom_segment(aes(x=index, xend=index, y=0, yend=scores)) +
      geom_vline(xintercept = 0, col="red", linetype="dotted") +
      geom_vline(xintercept = x$report$offset, col="red",
                 linetype="dotted") +
      xlab("X/Y Lag [Rows]") +
      ylab("Correlation [R]") +
      theme_bw()
    if(x$report$offset!=0){
#this should be tidied
#arrow not good
#could add text saying offset?
      ref <- max(df$scores, na.rm=TRUE)
      plt <- plt +
        geom_segment(x=x$offset, xend=0, y=ref, yend=ref,
          arrow = arrow(length=unit(0.30,"cm"),
                  ends="first", type = "closed"),
          col="red")
    }
    plt
  }

#alignment_corAlignmentPlot <-
#  function(x, ...){
#    lattice::xyplot(x$reports$scores~x$reports$index,
#                    type="h", xlab = "X/Y Lag [Rows]", ylab = "Correlation [R]",
#                    panel = function(...){
#                      lattice::panel.grid(-1, -1)
#                      lattice::panel.xyplot(...)
#                      lattice::panel.abline(v = 0, col.line = "red", lty = 3)
#                      lattice::panel.abline(v = x$report$offset, col = "red",
#                                            lty = 3)
#                      if(x$report$offset!=0)
#                        lattice::panel.arrows(0, max(x$reports$scores,
#                                                     na.rm = TRUE),
#                                              x$reports$offset,
#                                              max(x$reports$scores, na.rm = TRUE),
#                                              col = "red", 0.1)
#                    })
#  }
