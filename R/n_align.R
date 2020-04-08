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
#' @param n \code{y} offset when binding \code{y} to \code{x},
#' default 0.
#' @param by If \code{x} or \code{y} are \code{data.frame}s, the names
#' of the columns that \code{n_align} should align.
#' @param ... Other arguments, currently ignored.
#' @author Karl Ropkins
#' @return \code{data.frame} of \code{x} and \code{y}, with \code{y}
#' offset \code{n} rows.

#splatted function (2019/06/07)
#based on align in pems.utils
#renamed because package name align
#    and this is not main function...

############################
#testing
############################
#made n_align a method
#incorporated align output handling
#made alignment output
#    alignment methods: currently print only...
#added by
#    (all currently messy)


#' @rdname n_align
#' @export
n_align <-
  function(x, y = NULL, n = 0, by = NULL, ...) {
    UseMethod("n_align")
  }


#' @export
#' @method n_align default
n_align.default <-
n_align <-
  function(x, y = NULL, n = 0, by = NULL, ...){
    #same as previously align
    #    without pems class handling and INSTEAD
    #    including align package structure

    #####################################
    #unexported align_extraArgsHandler
    #####################################
    #options
    #Done:
    #To consider doing: "ans","plot", "offset", "summary", "alignment"
    x.args <- align_extraArgsHandler(...,
                default.method = "n_align",
                default.output = c("ans"),
                ref.args = c("ans", "alignment"))

    ####################################
    #unexported align_XYByArgsHandler
    ####################################
    ## can't do this yet
    d <- align_XYByArgsHandler(x=x, y=y, by=by,
                               method = x.args$method)
    x <- d$x
    y <- d$y
    #x <- as.data.frame(x, stringsAsFactors = FALSE)
    #if(!is.null(y)) {
    #  y <- as.data.frame(y, stringsAsFactors = FALSE)
    #} else {
    #  if(!is.null(by)){
    #    by <- c(names(by), by)
    #    if(by[1] %in% names(x)){
    #      y <- as.data.frame(x[by[1]], stringsAsFactors = FALSE)
    #      x <- as.data.frame(x[names(x) != by[1]],
    #                         stringsAsFactors = FALSE)
    #    } else {
    #      stop("..._align(x, by, ...) missing 'y' or 'by' element",
    #           call. = FALSE)
    #    }
    #  } else {
    #    stop("..._align(x, by, ...) missing 'x' or 'by' element",
    #         call. = FALSE)
    #  }
    #}

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
    temp <- min(ans$..ref, na.rm = TRUE): max(ans$..ref,
                                              na.rm = TRUE)
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
    ans <- ans[names(ans)!="..ref"]

    #make alignment object
    #    align_buildAlignment in unexported code
    #    (if objects do not get anymore complicated
    #        should probably drop function and do directly)
    alignment <- align_buildAlignment(method = "n_align",
                                      ans = ans,
                                      sources = list(x = x,
                                                     y = y),
                                      offset = n)

    #output
    align_output(alignment, x.args$output)
  }
