############################################
#' @title alignment
############################################

#' @name alignment
#' @description Generic methods for use with \code{alignment} class
#' objects generated with \code{..._align(...,  output = "alignment")}.
#' @param x,object \code{alignment} class object
#' @param ... Other arguments
#' @aliases plot.alignment print.alignment summary.alignment

################################
#contents (all crude)
################################
#exported
################################
#plot.alignment
#print.alignment
#summary.alignment
################################
#unexported code
################################
#[to list]

############################
#to do
############################
#tidy documentation
# describe usage and object handling...
#tidy all alignment generics (currently very rough)
#work up for other ..._align functions (currently just cAlign)
#could put all the alignment object handling on one page
#    or on same page as parent ..._align() is we use dedicated
#    alignment objects, so n_alignment, cor_alignment, etc?

############################
#to think about
############################
#converted from if to switch structure
#    might be better, faster option
#    will still need more tidying once outputs finished
#    testing on print.alignment
#summary is not really a summary...


##############################
#plot.alignment
##############################
#plot functions are in _align .r files

#splatted function
#' @rdname alignment
#' @export
#' @method plot alignment
plot.alignment <-
function(x, ...){
  #plot method for alignment object
  switch(x$method,
#cor plot function name needs fixing
         cor_align = alignment_corAlignmentPlot(x, ...),
         cow_align = alignment_cowAlignmentPlot(x, ...),
         fit_align = alignment_fitAlignmentPlot(x, ...),
         {cat("[alignment: no matched plot method]\n");
          invisible(x)}
  )
}

##################
#print.alignment
###################
#minimal for cor_alignment
#minimal for n_alignment
#minimal for cow_alignment

#' @rdname alignment
#' @method print alignment
#' @export
print.alignment <-
  function(x, ...){
    #using switch statement, one per ..._align method
    ans <- switch(x$method,
        cor_align = paste("alignment: xy cor_alignment: ",
                    x$offset, " row offset", sep=""),
        cow_align = paste("alignment: xy cow_alignment: ",
                          length(x$reports[[1]]), " y segment warp [",
                          x$reports[[5]][1], " to ",
                          x$reports[[5]][2], "]", sep=""),
        fit_align = paste("alignment: xy fit_alignment: ",
                          paste(deparse(x$fun), collapse = ""),
                          sep=""),
        n_align = paste("alignment: xy n_alignment: ",
                          x$offset, " row offset", sep=""),
        "[alignment: no plot method]"
    )
    cat(ans, "\n")
    invisible(x)
  }


##################
#summary.alignment
###################
#minimal for cor_alignment
#minimal for cow_alignment

#notes:
#no n_align method

#' @rdname alignment
#' @method summary alignment
#' @export
summary.alignment <-
  function(object, ...){
    #summary method for alignment object
    if(object$method == "cor_align"){
      #quick for now
      #think about this
      cat("alignment: cor_align\n",
          "\tx (", nrow(object$sources$x0), " x ",
          ncol(object$sources$x0), "); ",
          "y (", nrow(object$sources$y0), " x ",
          ncol(object$sources$y0), ")\n",
          "\ty row offset: ", object$offset, "\n",
          "\txy (", nrow(object$ans), " x ",
          ncol(object$ans), ")\n", sep="")
      return(invisible(object))
    }
    if(object$method == "cow_align"){
      #quick for now
      #think about this
      temp <- paste(length(object$reports[[1]]), " SEG warped [",
                    object$reports[[5]][1], " - ",
                    object$reports[[5]][2], "]", sep="")
      if(is.null(object$offset) || object$offset!=0) {
        temp <- paste("offset ", object$offset, "; ", temp, sep="")
      }
      cat("alignment: cow_align\n",
          "\tx (", nrow(object$sources$x0), " x ",
          ncol(object$sources$x0), "); ",
          "y (", nrow(object$sources$y0), " x ",
          ncol(object$sources$y0), ")\n",
          "\tcow: ", temp, "\n", "\txy (",
          nrow(object$ans), " x ", ncol(object$ans),
          ")\n", sep="")
      return(invisible(object))
    }
    if(object$method == "fit_align"){
      #quick for now
      #think about this
      temp <- paste(object$reports$optim$bestmem,
                    collapse = ",", sep="")
      cat("alignment: fit_align\n",
          "\tx (", nrow(object$sources$x0), " x ",
          ncol(object$sources$x0), "); ",
          "y (", nrow(object$sources$y0), " x ",
          ncol(object$sources$y0), ")\n",
          "\ty warp function: ",
          deparse(object$fun), "\n\t[", temp, "]\n",
          sep="")
      return(invisible(object))
    }
    #warn if no method?
    cat("[alignment: no matched summary method]\n")
    invisible(NULL)
  }



####################################
#misc unexported code
####################################


###################################
#align_extraArgsHandler
###################################

#kr v 0.0.1 (2019/06/05)

###############################
#to do
###############################
#work in progress - structure to be
#    finalised


align_extraArgsHandler <-
  function(..., default.method = "..._align",
           default.output = c("ans", "plot"),
           ref.args = c("ans", "plot", "alignment")
  ){

    #handles bad method requestes
    #rationalises output = "plot" and plot = TRUE, etc
    extra.args <- list(...)
    #make output if not there

    if(!"method" %in% names(extra.args))
      extra.args$method <- default.method
    if(!extra.args$method[1] %in% c("n_align", "cor_align",
                                    "cow_align", "fit_align")){
      #not sure this should be user option...
      stop(paste("no '", extra.args$method[1], "' alignment method",
                 sep = ""), call. = FALSE)
    }
    if(!"output" %in% names(extra.args))
      extra.args$output <- default.output
    #fill if all
    if("all" %in% extra.args$output)
      extra.args$output <- ref.args
    #handle all the plot=TRUE, etc...
    if(length(ref.args)>0){
      for(i in ref.args){
        if(i %in% names(extra.args)){
          if(is.logical(extra.args[[i]])){
            extra.args$output <- if(extra.args[[i]])
              unique(c(extra.args$output, i)) else
                extra.args$output[extra.args$output != i]
            extra.args[[i]] <- NULL
          }
        }
      }
    }

    #ouput
    extra.args
  }


######################################
#align_XYByArgsHandler
######################################

#kr v 0.0.1 (2019/06/17)

######################################
#to do
######################################
#rationalise x,y, by handling code once
#    methods confirmed
#work in progress
#

#' @importFrom dplyr intersect

align_XYByArgsHandler <-
  function(x, y = NULL, by = NULL, method = "..._align"){

    ####################################################
    #common x, y and by handling for ....nalign functions
    ####################################################

    #testing sources

    #x missing?

    #x and y can are (or can be) data.frames
    if("try-error" %in% class(try(as.data.frame(x), silent=TRUE)))
      stop(paste("no '", method, "' method for x of class ",
                 class(x)), call. = FALSE)
    if("try-error" %in% class(try(as.data.frame(y), silent=TRUE)))
      stop(paste("no '", methods, "' method for y of class ",
                 class(y)), call. = FALSE)

    #assuming all the same expect n_align
    if(method == "n_align"){
      x <- as.data.frame(x, stringsAsFactors = FALSE)
      if(!is.null(y)) {
        y <- as.data.frame(y, stringsAsFactors = FALSE)
      } else {
        if(!is.null(by)){
          by <- c(names(by), by)
          if(by[1] %in% names(x)){
            y <- as.data.frame(x[by[1]], stringsAsFactors = FALSE)
            x <- as.data.frame(x[names(x) != by[1]],
                               stringsAsFactors = FALSE)
          } else {
            stop("n_align(x, by, ...) missing 'y' or 'by' element",
                 call. = FALSE)
          }
        } else {
          stop("n_align(x, by, ...) missing 'y' or 'by' element",
               call. = FALSE)
        }
      }
      return(list(x=x, y=y))
    }

    #y & by missing stop
    #could default to taking first two columns on x?
    if(is.null(y) & is.null(by)){
      stop(method, "(x, ...) requires 'y', 'by' or both...",
           call. = FALSE)
    }

    #input combinations
    ######################################
    #should rationalise this later
    ######################################
    if(!is.null(by)){
      #get names from by
      by <- c(names(by), by)
      if(is.null(y)){
        #have x & by
        #two by names should be in x
        if(length(by) > 1 && all(by %in% names(x))){
          return(list(
            x = x[,by[1]],
            y = x[,by[2]],
            x0 = as.data.frame(x[names(x) != by[2]],
                               stringsAsFactors = FALSE),
            y0 = as.data.frame(x[by[2]], stringsAsFactors = FALSE)
          ))
        } else {
          stop(method, "(x, by, ...) missing 'x' or 'by' element",
               call. = FALSE)
        }
      } else {
        #have x, y and by
        #if only 1 by should be in both...
        if(length(by) == 1 && by %in% names(x) && by %in% names(y)){
          return(list(
            x = x[,by],
            y = y[,by],
            x0 = as.data.frame(x, stringsAsFactors = FALSE),
            y0 = as.data.frame(y, stringsAsFactors = FALSE)
          ))
        }
        #if only 1 by should be in one and other should be 1d...
        if(length(by) == 1 && by %in% names(x) && ncol(as.data.frame(y))==1){
          y <- as.data.frame(y, stringsAsFactors = FALSE)
          return(list(
            x = x[,by],
            y = y[,1],
            x0 = as.data.frame(x, stringsAsFactors = FALSE),
            y0 = y
          ))
        }
        if(length(by) == 1 && by %in% names(y) && ncol(as.data.frame(x))==1){
          x <- as.data.frame(x, stringsAsFactors = FALSE)
          return(list(
            x = x[,1],
            y = y[,by],
            x0 = x,
            y0 = as.data.frame(y, stringsAsFactors = FALSE)
          ))
        }
        #if 2 in by first should be in x, second in y
        if(length(by) > 1 && by[1] %in% names(x) && by[2] %in% names(y)){
          return(list(
            x = x[,by[1]],
            y = y[,by[2]],
            x0 = as.data.frame(x, stringsAsFactors = FALSE),
            y0 = as.data.frame(y, stringsAsFactors = FALSE)
          ))
        }
        #terminate because should not be here...
        stop(method, "(x, y, by, ...) 'x', 'y', 'by' mismatch",
             call. = FALSE)
      }
    } else {
      #have x and y
      #if both 1d can use...
      if(ncol(as.data.frame(x))==1 && ncol(as.data.frame(y))==1){
        x <- as.data.frame(x, stringsAsFactors = FALSE)
        y <- as.data.frame(y, stringsAsFactors = FALSE)
        return(list(
          x = x[,1],
          y = y[,1],
          x0 = x,
          y0 = y
        ))
      }
      #if have intersecting name can use...
      if(length(dplyr::intersect(names(x), names(y))) > 0){
        ref <- dplyr::intersect(names(x), names(y))[1]
        warning(method, "(x, y, ...) using first 'x'/'y' match, by='",
                ref, "'.",
                call. = FALSE)
        return(list(
          x = x[,ref],
          y = y[,ref],
          x0 = as.data.frame(x, stringsAsFactors = FALSE),
          y0 = as.data.frame(y, stringsAsFactors = FALSE)
        ))
      }
      #terminate should not get to here
      stop(method, "(x, y, ...) targets unclear, maybe specify 'by'?",
           call. = FALSE)
    }
  }



###################################
#align_buildAlignment
###################################

#kr v.0.0.1 (2019/06/17)

###############################
#to do
###############################
#work in progress - structure to be
#    finalised
#

align_buildAlignment <-
  function(...){
    #build alignment object
    object <- list(...)
    class(object) <- "alignment"
    object
  }


######################################
#align_output
######################################

#kr v.0.0.1 (2019/06/18)

###############################
#to do
###############################
#work in progress - structure to be
#    finalised
#

align_output <- function(alignment, output){
  #standard output handler
  if(length(output) > 0){
    #send later outputs as print()s
    #send last as function return
    #####################################
    #needs tidying
    #####################################
    for(i in 1:length(output)){
      if(i == length(output)){
        if(output[i] == "plot") return(plot(alignment))
        if(output[i] == "summary") return(summary(alignment))
        if(output[i] == "print") print(alignment)
        if(output[i] == "alignment") return(alignment)
        if(output[i] %in% names(alignment)) return(alignment[[output[i]]])
      } else {
        if(output[i] == "plot") print(plot(alignment))
        if(output[i] == "summary") summary(alignment)
        if(output[i] == "print") print(alignment)
        if(output[i] == "alignment") alignment
        if(output[i] %in% names(alignment)) alignment[[output[i]]]
      }
    }
  }
}








