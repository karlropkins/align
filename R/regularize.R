############################################
#' @title regularize
############################################

#' @name regularize
#' @description regularizes a supplied \code{vector} or \code{data.frame}
#' on basis on an supplied counter, timestamp, etc.
#' @param x the supplied \code{vector} or \code{data.frame}.
#' @param ts vector of sample times, one per row of \code{x}.
#' @param Hz The (regular) resolution of the output, default \code{1}
#' generates one per second.
#' @param ... Other arguments, currently ignored.
#' @note If \code{counter} is numeric, it is assumed to be reported in
#' 1 second units.
#' @author Karl Ropkins


####################################
#to do
####################################
#think about arg structure
#think about warp_frame, currently not
#    exported
#

#splatted function
#' @export
regularize <-
function (x, ts = NULL, Hz = 1, ...)
{
    #this is regularize from sleeper.service
    #    then pems.utils
    ##############################################
    #de-pems.utils-ing...
    x <- as.data.frame(x)
    if (is.null(ts))
        stop("(required) 'ts' not supplied")
    #next bit is messy... see
    #    regularize(1:10, c(1:5, 7:11))
    #works but is not right...
    new.x <- (floor(ts[1]):ceiling(ts[length(ts)]))
    new.x <- ((1:(length(new.x)*Hz))/Hz) - 1 + floor(ts[1])
    #main section of sleeper.service::regularize was
    #same as main section of grey.area::warp
    #so using common unexported code...
    warp_frame(x, ts, new.x)
}
