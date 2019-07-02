####################################
#unexported code
####################################

#(see indivdual to do)



####################################
#warp_frame
####################################

#kr v 0.0.1 (2019/06/05)

###############################
#to do
###############################
#

#the warp_engine joke got very old, very quick...
#based on common code for regularize and nAlign in pems.utils
#and warp

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
        new.df[, i][which(new.x %in% old.x)] <- x[, i]
        for (j in 2:length(new.df[, i]))
          new.df[j, i] <- new.df[j - 1, i]
      }
      if (any(c("POSIXct", "POSIXt") %in% class(x[, i]))) {
        new.df[, i] <- x[1, i] + new.x
      }
    }
    new.df
  }


###################################
#align_extraArgsHandler
###################################

#kr v 0.0.1 (2019/06/05)

###############################
#to do
###############################
#work in progress - structure to be
#    finalised
#

align_extraArgsHandler <-
  function(..., default.output = c("ans", "plot"),
        ref.args = c("ans", "plot", "alignment")
){

  #rationalises output = "plot" and plot = TRUE, etc
  extra.args <- list(...)
  #make output if not there
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
#work in progress - structure to be
#    finalised
#

#' @importFrom dplyr intersect

align_XYByArgsHandler <-
  function(x, y = NULL, by = NULL){

  ####################################################
  #common x, y and by handling for ...Align functions
  ####################################################

  #x missing?

  #x and y can are (or can be) data.frames
  if("try-error" %in% class(try(as.data.frame(x), silent=TRUE)))
    stop(paste("no '..._align' method for x of class ",
               class(x)), call. = FALSE)
  if("try-error" %in% class(try(as.data.frame(y), silent=TRUE)))
    stop(paste("no '..._align' method for y of class ",
                class(y)), call. = FALSE)

  #y & by missing stop
  #could default to taking first two columns on x?
  if(is.null(y) & is.null(by)){
    stop("..._align(x, ...) requires 'y', 'by' or both...",
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
        stop("..._align(x, by, ...) missing 'x' or 'by' element",
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
          y0 = as.data.frame(x, stringsAsFactors = FALSE)
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
      stop("..._align(x, y, by, ...) 'x', 'y', 'by' mismatch",
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
      warning("..._align(x, y, ...) using first 'x'/'y' match, by='",
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
    stop("..._align(x, y, ...) targets unclear, maybe specify 'by'?",
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



##################################
#alignment_corAlignPlot
##################################

#kr v.0.0.2
#update of previous
#using lattice::xyplot rather than plot
#  not sure I need the importfrom if I am using lattice::fun()
#  might change this to ggplot but dependents and installation
#      time will increase significantly
############################
#to do
############################
#code needs tidying to allow user modification
#

#' @importFrom lattice xyplot
alignment_corrAlignmentPlot <-
  function(x, ...){
    lattice::xyplot(x$reports$scores~x$reports$index,
      type="h", xlab = "X/Y Lag [Rows]", ylab = "Correlation [R]",
      panel = function(...){
          lattice::panel.grid(-1, -1)
          lattice::panel.xyplot(...)
          lattice::panel.abline(v = 0, col.line = "red", lty = 3)
          lattice::panel.abline(v = x$report$offset, col = "red",
                lty = 3)
          if(x$report$offset!=0)
              lattice::panel.arrows(0, max(x$reports$scores,
                                           na.rm = TRUE),
                x$reports$offset,
                max(x$reports$scores, na.rm = TRUE),
                col = "red", 0.1)
      })
  }
