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
                  default.output = c("summary", "plot", "ans"),
                  ref.args = c("ans","plot", "offset",
                               "summary", "alignment"))

    #x, y and by general output handler
    #align_XYByArgsHandler in unexported code
    ###########################
    #note
    ###########################
    #this currently limits
    #cor_align to valid
    #data.frames and as.data.frame()s
    ###########################
    #to do
    ###########################
    #handle missing x?
    #return aligned.y?
    d <- align_XYByArgsHandler(x=x, y=y, by=by)

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
    #   0.1, 0.2, so not ideal but good trade-off.

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
    ans <- nAlign(d$x0, d$y0, n = bst.lag, output = "ans")

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


