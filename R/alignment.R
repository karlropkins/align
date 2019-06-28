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
#plot.alignment
#print.alignment
#summary.alignment

############################
#to do
############################
#tidy documentation
#tidy all alignment generics (currently very rough)
#work up for other ..._align functions (currently just cAlign)
#could put all the alignment object handling on one page
#    or on same page as parent ..._align() is we use dedicated
#    alignment objects, so nAlignment, cAlignment, etc?
#could convert from if to switch structure and error out
#    if not known method..?

#splatted function
#' @rdname alignment
#' @export
#' @method plot alignment

#minimal for cor_alignment only
plot.alignment <-
function(x, ...){
  #plot method for alignment object
  if(x$method == "cor_align"){
    ##################################################
    #replace this with lattice plot or ggplot
    #so ouput is actual plot rather an invisible(x)...
    ##################################################
    plot(x$reports$index, x$reports$scores, type="h",
         xlab = "X/Y Lag [Rows]", ylab = "Correlation [R]")
    abline(v=0, col="pink", lty=3)
    abline(v=x$report$offset, col="red", lty=3)
    if(x$report$offset!=0)
      arrows(0, max(x$reports$scores, na.rm=T), x$reports$offset ,
             max(x$reports$scores, na.rm=T), col="red", 0.1)
    invisible(x)
  }
}


#' @rdname alignment
#' @method print alignment
#' @export

#minimal for cor_alignment only
print.alignment <-
  function(x, ...){
    #summary method for alignment object
    if(x$method == "cor_align"){
      #quick for now
      cat("alignment: xy cor_alignment row offset: ", x$offset, "\n")
      invisible(x)
    }
  }


#' @rdname alignment
#' @method summary alignment
#' @export

#minimal for cor_alignment only
summary.alignment <-
  function(object, ...){
    #summary method for alignment object
    if(object$method == "cor_align"){
      #quick for now
      #think about this
      cat("alignment: cor_align\n",
          "\tx (", nrow(object$sources$x0), "x",
          ncol(object$sources$x0), ");\n",
          "\ty (", nrow(object$sources$y0), "x",
          ncol(object$sources$y0), ");\n",
          "\ty row offset: ", object$offset, "\n")
      invisible(object)
    }
    #warn if no method?
    invisible(NULL)
  }
