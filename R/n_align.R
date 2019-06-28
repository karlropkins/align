############################################
#' @title n_align
############################################

#' @name n_align
#' @description Basic alignment, row offsetting second of two
#' \code{vector}s or \code{data.frame}s.
#' @param x First \code{vector} or \code{data.frame}, to be aligned
#' and merged with.
#' @param y Second \code{vector} or \code{data.frame} to align and
#' merge with \code{x}.
#' @param n \code{y} offset when binding \code{y} to \code{x},  default 0.
#' @param ... Other arguments, currently ignored.
#' @author Karl Ropkins
#' @return \code{data.frame} of \code{x} and \code{y}, with \code{y}
#' offset \code{n} rows.

#splatted function (2019/06/07)
#based on align in pems.utils
#renamed because package name align
#    and this is not main function...

############################
#to do
############################
#non-unique name handling
#no name handling (?)
#safer alternative to $..ref
#look at unexported code
#examples
#think about common handling
#   (1) by would be meaningless
#   except maybe n_align(x, by="column.in.x", n=2)
#   (2) an alignment output from nAlign
#   would be of very limited value...
#

#' @importFrom dplyr full_join
#' @export
n_align <-
  function(x, y, n = 0, ...){
    #same as previously align
    #    but without pems class handling
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    #could change non-unique name handling?
    #######################################
    ##temp <- make.names(c(names(x), names(y)), unique = TRUE)
    #######################################
    #what if not named
    #(could not happen in pems.utils)
    #..ref was longer/less likely to get used
    #    in pems.utils version
    x$..ref <- 1:nrow(x)
    y$..ref <- 1:nrow(y) + n
    ans <- dplyr::full_join(x, y, by="..ref")
    ####################
    #pad ref if needed
    #(pems did this for you)
    ####################
    temp <- min(ans$..ref, na.rm = TRUE): max(ans$..ref, na.rm = TRUE)
    temp <- temp[!temp %in% ans$..ref]
    if(length(temp)>0){
      ref <- (nrow(ans)+1):(nrow(ans)+length(temp))
      ans[ref,] <- NA
      ans$..ref[ref] <- temp
    }
    ##################
    #order, tidy and return
    ##################
    ans <- ans[order(ans$..ref),]
    rownames(ans) <- 1:nrow(ans)
    ans[names(ans)!="..ref"]
  }
