############################################
#' @title cowAlign
############################################

#' @name cowAlign
#' @description Non-linear alignment using Correlation Optimized
#' warping (COW). This is \code{cow} but with argument structure like
#' other ...Align functions.
#' @param x First \code{vector}, to be COW aligned with when merged
#' data.
#' @param y Second \code{vector} to COW warp align with \code{x}.
#' @param seg Segment size for warping.
#' @param slack Segment size expansion/compression range.
#' @param print.report (logical) print segment report.
#' @param r.power (logical or numeric) correlation power
#' (1-4).
#' @param equal.segments (logical) force equal segment lengths in
#' \code{xt} and \code{xP} instead of filling up \code{xt} with N
#' boundary-points.
#' @param ... Other arguments, currently ignored.
#' @note This function is based on previous matlab (see Source),
#' but some formal arguments and parameters have been changed to
#' make function more consistent with other \code{align} functions.
#' @author Daniel Quiroz Moreno.
#' @source Based on matlab code at http://www.models.life.ku.dk/dtw_cow.
#' @references Correlation optimized warping was developed as a
#' preprocessing method for chromatographic Data:
#'
#' Niels-Peter Vest Nielsen, Jens Micheal Carstensen
#' and Jørn Smedegaard,  1998, Aligning of singel and multiple wavelength
#' chromatographic profiles for chemometric data analysis using
#' correlation optimised warping. J. Chrom. A 805(1998), 17-35
#'
#' Giorgio Tomasi, Frans van den Berg and Claus Andersson, 2004,
#' Correlation optimized warping and dynamic time warping as
#' preprocessing methods for chromatographic Data, Journal of
#' Chemometrics 18 (2004), 231-241.


#splatted function
#Daniel Quiroz Moreno code
#emailed 2019/06/04

###############################
#code as provided by Daniel
#BUT modified to make it more like
#other ...Align functions
###############################

###############################
#to do
###############################
#fix reference (character corrupted)
#think about data.frame by and output
#   handling like cAlign
#think about alignment output, plot,
#   summary outputs

###############################
#from previous help
###############################
# in:  T (1 x nt) target vector
#      X (mP x nP) matrix with data for mP row vectors of length nP to be warped/corrected
#      Seg (1 x 1) segment length; number of segments N = floor(nP/m)
#       or (2 x N+1) matrix with segment (pre-determined) boundary-points
#                    first row = index in "xt", must start with 1 and end with "nt"
#                    second row = index in "xP", must start with 1 and end with "nP"
#      slack (1 x 1) 'slack' - maximum range or degree of warping in segment length "m"
#
#      options (1 x 5) 1 : triggers plot and progress-text (note: only last row/object in "xP" is plotted)
#                      2 : correlation power (minimum 1th power, maximum is 4th power)
#                      3 : force equal segment lengths in "xt" and "xP" instead of filling up "xt" with N boundary-points
#                          (notice that different number of boundaries in "xt" and "xP" will generate an error)
#                      4 : fix maximum correction to + or - options(4) points from the diagonal
#                      5 : save in "diagnos" the table with the optimal values of loss function and predecessor (memory
#                                                                                                                 %                          consuming for large problems - on how to read the tables are in the m-file
#                default [0 1 0 0 0] (no plot; power 1; no forced equal segment lengths; no band constraints; no Table in "diagnos")
#
#     % out: Warping (mP x N x 2) interpolation segment starting points (in "nP"
#     %          units) after warping (first slab) and before warping (second slab)
#     %          (difference of the two = alignment by repositioning segment
#     %          boundaries; useful for comparing correction in different/new objects/samples)
#     %      XWarped (mP x nt) corrected vectors (from "xP" warped to mach "xt")
#     %      Diagnos (struct) warping diagnostics: options, segment, slack,
#     %          index in target ("xt", "warping" is shift compared to this) and sample ("xP"), search range in "xP", computation time
#     %          (note: diagnostics are only saved for one - the last - signal in "xP")
#
#     % based on: Niels-Peter Vest Nielsen, Jens Micheal Carstensen and Jørn Smedegaard 'Aligning of singel and multiple
#       wavelength chromatographic profiles for chemometric data analysis using correlation optimised warping'
#       J. Chrom. A 805(1998)17-35
#
#     % Reference: Correlation optimized warping and dynamic time warping as preprocessing methods for chromatographic Data
#     %            Giorgio Tomasi, Frans van den Berg and Claus Andersson, Journal of Chemometrics 18(2004)231-241
#

###############################
#changes cow -> cowAlign
###############################
#Ta to x,
#X to y,
#dimX to dimY,
#changed Xwarped to Ywarped
#Seg to seg,
#Slack to slack
#replaced option[1] with print.report
#replaced option[2] with r.power, can be 1-4
#replaced option[3] with equal.segments
#option 4 not done...
#does not seem to be an option 5...

###############################
#note:
# this errors out if option3/equal.segments set...
###############################

#' @export
cowAlign <- function(x, y, seg, slack, print.report = FALSE,
                    r.power = FALSE, equal.segments = FALSE,
                     ...) {

  #my changes to code need checking and tidying

  # Initialise
  if(is.matrix(y)){
    dimY <- length(y)  #  dimY   : Number of signals that are to be aligned
  } else {
    dimY <- c(1, length(y))
  }

  npT <- length(x) #  npT    : Number of points in the target
  Ywarped <- matrix(nrow = dimY[1], ncol = dimY[2]) # Initialise matrix of warped signasl
  time <- NULL

  #### Initialise segments ####
  seg <- floor(seg)
  pred_bound <- length(seg) > 1
  if (pred_bound){
    if ( seg == 1 & seg[length(seg)] == length(x) ) {
      stop('End points must be equal to 1 and to the length of x')
    }
    len_segs <- diff(seg)
    if (!all(len_segs > 2)) {
      stop('Segments must contain at least two points')
    }
    nSeg <- length(len_segs)
  } else{
    if (seg > min(c(dimY[2], npT))) {
      stop('Segment length is longer than x')
    }
    if (equal.segments) {
      nSeg <- floor((npT - 1)/seg)
      len_segs <- matrix(floor((npT - 1)/ nSeg), nrow = 1)
      len_segs[2, ] <- floor((dimY[2] - 1)/ nSeg)

      print('Segment length adjusted to the best cover the remainders')
    } else {
      nSeg = floor((npT - 1) / (seg - 1))
      tmp_segs <- rep(seg - 1, nSeg)
      len_segs <- matrix(c(tmp_segs, tmp_segs), nrow = 2)

      if (floor((dimY[2] -1) / (seg - 1)) != nSeg){
        stop('For non-fixed segment lengths x and y
             do not have the same number of segments. Try
             equal.segments = TRUE')
      }
    }
    tmp <- (npT-1) %% len_segs[1, 1]
    if( tmp > 0) {
      len_segs[1, nSeg] <- len_segs[1, nSeg] + tmp
      if(print.report) {
        print(paste0('Segments: ', len_segs[1,1] + 1,
                     ' Points: ', nSeg -1))
      }
    } else {
      if (print.report) {
        print(paste0('Segments: ', len_segs[2, 1] + 1,
                     ' Points x: ', nSeg))
      }
    }

    tmp <- (dimY[2] - 1) %% len_segs[2, 1]
    if(tmp > 0) {
      len_segs[2, nSeg] <- len_segs[2, 1] + tmp
    }
  }
  bT <- cumsum(c(1, len_segs[1, ]))
  bP <- cumsum(c(1, len_segs[2, ]))
  Warping <- matrix(nrow = dimY[1], ncol = (nSeg+1) )

  #### Chech slack ####
  if (length(slack) > 1){
    if (length(slack) <= nSeg) {
      stop('The number of slack parameters is not equal to the number of optimised segments')
    }
    stop('Multiple slacks have not been implemented yet')
  }
  Slacks_vec = seq(-slack, slack)

  #### Set feasible points for boundaies ####
  Bounds <- matrix(rep(1, 2*(nSeg +1)), nrow = 2)
  # Slope constrints
  offs_tmp <- slack * seq(0, nSeg)
  offs <- matrix(c(-offs_tmp, offs_tmp), nrow = 2, byrow = T)
  Bounds_ind <- seq(1, nSeg +1)
  Bounds_a2 <- matrix(rep(bP[Bounds_ind], 2), nrow = 2, byrow = T)
  Bounds_a <- Bounds_a2 + offs
  offs_tmpb <- matrix(c(-rev(offs_tmp), rev(offs_tmp)), nrow = 2, byrow = T)
  Bounds_b <- Bounds_a2 + offs_tmpb
  Bounds[1, ] <- apply(matrix(c(Bounds_a[1, ],Bounds_b[1, ]),
                              nrow = 2, byrow = T), 2, max )
  Bounds[2, ] <- apply(matrix(c(Bounds_a[2, ], Bounds_b[2, ]),
                              nrow = 2, byrow = T), 2, min )

  #Option 4 is not incorporated

  #### Calculate de first derivate for interpolation ####
  Ydiff <- diff(y)

  ####  Calculate coefficients and indexes for interpolation ####
  Int_Coeff <- vector('list', nSeg)
  Int_Index <- Int_Coeff

  if(!pred_bound) {
    for (i in seq(1, nSeg -1 )) {
      Int_Coeff[[ i ]] <- InterpCoeff(n = len_segs[1, 1] +1 ,
                                      nprime = len_segs[2, 1 ] + Slacks_vec + 1,
                                      offs = Slacks_vec, rtn = 'coeff')

      Int_Index[[ i ]] <- InterpCoeff(n = len_segs[1, 1] +1 ,
                                      nprime = len_segs[2, 1 ] + Slacks_vec + 1,
                                      offs = Slacks_vec, rtn = 'index')
    }

    Int_Coeff[[nSeg]] <- InterpCoeff(n = len_segs[1, nSeg] +1 ,
                                     nprime = len_segs[2, nSeg ] + Slacks_vec + 1,
                                     offs = Slacks_vec, rtn = 'coeff')

    Int_Index[[nSeg]] <- InterpCoeff(n = len_segs[1, nSeg] +1 ,
                                     nprime = len_segs[2, nSeg ] + Slacks_vec + 1,
                                     offs = Slacks_vec, rtn = 'index')

  } else {
    for (i in seq(1, nSeg) ) {
      Int_Coeff[[i]] <- InterpCoeff(n = len_segs[1, i] +1 ,
                                    nprime = len_segs[2, i ] + Slacks_vec + 1,
                                    offs = Slacks_vec, rtn = 'coeff')

      Int_Index[[i]] <- InterpCoeff(n = len_segs[1, i] +1 ,
                                    nprime = len_segs[2, i ] + Slacks_vec + 1,
                                    offs = Slacks_vec, rtn = 'index')
    }
  }


  #### Dynamic Programming Section ####
  table_index <- cumsum(c(0, Bounds[2, ] - Bounds[1, ] + 1) )
  Table <- matrix(0, nrow = 3, ncol =table_index[nSeg + 2])

  Table[2, 2:ncol(Table)] <- -Inf


  for (i in seq(1, nSeg + 1)) {
    v <- seq(Bounds[1, i], Bounds[2, i])
    Table[1, seq(table_index[i] +1, table_index[i +1])  ] <-v
  }
  # Forward phase
  time1 <- Sys.time()

  for (i in seq(1, nSeg)) {
    a <- Slacks_vec + len_segs[2, i]
    b <- table_index[i] + 1 - Bounds[1, i]
    c <- len_segs[1, i] + 1
    counting <- 1
    node_z <- table_index[i + 2]
    node_a <- table_index[i + 1] + 1
    bound_k_table <- matrix(nrow = 2, ncol =  node_z - node_a + 1)

    int_index_seg <- t(Int_Index[[i]]) - (len_segs[2, i] +1)
    int_coeff_seg <- t(Int_Coeff[[i]])

    TSeg = x[ seq(bT[i], bT[i + 1]) ];
    TSeg_centered = TSeg - sum(TSeg)/length(TSeg)
    Norm_TSeg_cen  <- norm(TSeg_centered, type = '2')

    for (j in seq(node_a, node_z) ) {
      prec_nodes <- Table[1, j] - a
      allowed_arcs <- prec_nodes >= Bounds[1, i] & prec_nodes <= Bounds[2, i]
      nodes_tablePointer <- b + prec_nodes[allowed_arcs]
      n_aa <- sum(allowed_arcs)

      if(n_aa  != 0 ){
        Index_Node <- Table[1, j] + int_index_seg[, allowed_arcs]
        coeff_b <- sapply(int_coeff_seg[, allowed_arcs], function(x) x)
        #coeff_b <- coeff_b[1, ]
        Yi_seg <- y[Index_Node]
        Yi_diff <- Ydiff[Index_Node]
        toreshape <- matrix(c(coeff_b, Yi_diff), nrow = 2, byrow = T)
        toreshape <- apply(toreshape, 2, prod) + Yi_seg
        Yi_seg <- matrix(toreshape, nrow = c, ncol = n_aa*dimY[1] )

        Yi_seg_mean <- colSums(Yi_seg)/nrow(Yi_seg)
        Norm_Yi_seg_cen <- sqrt(colSums(Yi_seg^2) - nrow(Yi_seg) * Yi_seg_mean^2)
        CCs_Node <- ( as.numeric(TSeg_centered) %*% Yi_seg ) /
          (Norm_TSeg_cen %*% Norm_Yi_seg_cen)
        CCs_Node <- ifelse(is.finite(CCs_Node), CCs_Node, 0)
        CCs_Node <- matrix(CCs_Node, nrow = n_aa, ncol = dimY[1])
########################
#check this is right way around
########################
        if(r.power){
          Cost_Fun = matrix(Table[2, nodes_tablePointer], nrow = n_aa ) + CCs_Node
        } else {
          Cost_Fun <- matrix(Table[2, nodes_tablePointer], nrow = n_aa) + CCs_Node^r.power
        }
#########################
        ind <- max(Cost_Fun)
        pos <- match(ind, Cost_Fun)
        bound_k_table[1, counting] <- ind
        bound_k_table[2, counting] <- nodes_tablePointer[pos]
        counting <- counting + 1
      }
    }
    Table[2:3, seq(node_a, node_z)] <- bound_k_table
  }
  time2 <- Sys.time()
  elapsed <- time2 - time1
  for (i in seq(1, dimY[1])) {
    Pointer <- ncol(Table)
    Warping[i, nSeg + 1] <- dimY[2]
    for (j in seq(nSeg, 1, -1)) {
      Pointer <- Table[3, Pointer]
      Warping[i, j] <- Table[1, Pointer]
    }
  }
  Ywarped <- NULL
  for (i in seq(1, nSeg)) {
    indT <- seq(bT[i], bT[i + 1])
    lenT <- bT[i + 1] - bT[i]
    for (j in seq(1, dimY[1])) {
      indX <- seq(Warping[j, i], Warping[j, i + 1])
      lenX <- Warping[j, i + 1] - Warping[j, i]
      Ywarped[indT] <- approx(x = indX - Warping[j, i] + 1,
                              y = y[indX],
                              xout = seq(0, lenT)/lenT * lenX + 1 )$y
    }
  }
  return(Ywarped)
}


#######################
#not exported
#    histc and InterpCoeff
#######################

histc <- function(values, edges) {
  ledges <- length(edges)
  nlow <-  length(values[values < edges[1]])
  nhigh <- length(values[values > edges[ledges]])
  bin <- NULL
  for (i in seq_along(edges)[-ledges] ) {
    binValues <- values[values >= edges[i] & values < edges[i+1]]
    position <- match(binValues, values)
    bin[position] <- i
  }
  bin <- bin[!is.na(bin)]
  upper <- match(edges[ledges], values)
  if(!is.na(upper)){
   bin[length(bin) +1 ] = i+1
  }
  if( nlow > 0 ){
    bin <- c(rep(0, nlow), bin)
  }
  if( nhigh > 0 ){
    bin <- c(bin, rep(0, nlow))
  }

  return(bin)
}

InterpCoeff <- function(n, nprime, offs, rtn) {
  p <- length(nprime)
  q <- n - 1
  coeff <- matrix(nrow = p, ncol =  n)
  index <- matrix(nrow = p, ncol =  n)
  for (i in seq(1, p)) {
    pp <- seq(1, nprime[i])
    p <- seq(0, q) * (nprime[i] - 1)/q +1
    k <- histc(p, pp)
    k[p >= nprime[i] ] <- nprime[i] -1
    coeff[i, ] <- ( p - pp[k] )
    index[i, ] <- k -offs[i]
  }
  switch(rtn,
         'coeff' = return(coeff),
         'index' = return(index))
}

