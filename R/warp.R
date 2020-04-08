############################################
#' @title warp
############################################

#' @name warp
#' @description Warps a supplied \code{vector} time-series or
#' \code{data.frame} using a warp transformation.
#' @param x \code{vector} time-series or \code{data.frame}, to be
#' warped
#' @param trans \code{numeric vector} the warp transformation to apply
#' to \code{x}.
#' @param ... Other arguments, currently ignored.
#' @return \code{x} warped, by default a \code{data.frame}.
#' @author Karl Ropkins
#' @note \code{warp}s are not conservative.
#' @seealso \code{\link{approx}}
#' @examples
#' x <- 1:1000
#' plot(x, type = "l", main = "Simple Warping")
#' #transformations < 1 compress time-series
#' lines(warp(x, 0.5), col = "red")
#' #transformations > 1 expand time-series
#' #to expand and the compress, and end at original end value
#' #   mean(transformation) should equal 1
#' lines(warp(x, c(1.75, 0.25)), col = "blue")
#' legend(725, 350, c("0.5", "1.75, 0.25"),
#'        col=c("red", "blue"), lty=1, cex=0.75)

#splatted function

#############################
#to do
#############################
#

#' @export
warp <-
function (x, trans = 1, ...)
{
    #simple data warping
    #just expands x up warp term
    x <- as.data.frame(x)
    #expand/foreshorten trans
    x.int <- ceiling(nrow(x)/length(trans))
    trans <- rep(trans, each=x.int)[1:nrow(x)]
    #make old and new x
    old.x <- cumsum(trans)
    new.x <- 1:ceiling(max(old.x, na.rm = TRUE))
    ###########################
    #earlier version
    ###########################
    #old.x <- 1:nrow(x)
    #new.x <- seq(1, max(old.x, na.rm = TRUE),
    #             length.out = length(old.x) * warp)
    ###########################
    #unexported function
    warp_frame(x, old.x, new.x)
}



#unexported code

####################################
#warp_frame
####################################

#kr v 0.0.1 (2019/06/05)

###############################
#to do
###############################
#

#the warp_engine joke got very old, very quick...
#based on common code for regularize and
#nAlign in pems.utils and warp

warp_frame <-
  function(x, old.x, new.x)
  {
    ###########################
    #this is a little messy
    #check final version in
    #sleeper.service was not better...
    ###########################
    new.df <- if(length(new.x) < length(old.x)){
      as.data.frame(x[1:length(new.x),])
    } else {
      temp <- as.data.frame(x)
      temp[length(new.x), 1] <- NA
      as.data.frame(temp)
    }
    names(new.df)[1] <- names(x)[1]
    #populate new.df
    for (i in names(x)) {
      ################################
      #need to think about not handled
      #object classes
      ################################
      if (is.numeric(x[, i]))
        new.df[, i] <- approx(old.x, x[, i], new.x, rule = 2)$y
      if (is.factor(x[, i]) || is.character(x[, i])) {
        #############################
        #factor correction needs
        #thinking about
        #############################
        new.df[, i][which(new.x %in% old.x)] <- x[, i][which(new.x %in% old.x)]
        for (j in 2:length(new.df[, i]))
        if(is.na(new.df[j, i])) new.df[j, i] <- new.df[j - 1, i]
      }
      if (any(c("POSIXct", "POSIXt") %in% class(x[, i]))) {
        new.df[, i] <- x[1, i] + new.x
      }
    }
    new.df
  }
