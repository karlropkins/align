############################################
#' @title cow
############################################

#' @name cow
#' @description Non-linear alignment using Correlation Optimized
#' warping.
#' @param Ta First \code{vector}, target for alignment.
#' @param X Second \code{vector} to be aligned with target.
#' @param Seg Segment size for warping.
#' @param Slack Segment size expansion/compression range.
#' @param Options Other options (to documet).
#' @author Daniel Quiroz Moreno
#' @note \code{cow} is based on similar matlab. (to document)
#' @references (to document)

#splatted function
#Daniel Quiroz Moreno code
#emailed 2019/06/04

###############################
#code as provided by Daniel
# currently just exporting cow
###############################

#' @export
cow <- function(Ta, X, Seg, Slack, Options) {

  # Initialise
  if(is.matrix(X)){
    dimX <-   length(X)  #  dimX   : Number of signals thar are to be aligned
  } else {
    dimX <- c(1, length(X))
  }

  npT <- length(Ta) #  npT    : Number of points in teh target
  Xwarped <- matrix(nrow = dimX[1], ncol = dimX[2]) # Initialise matrix of warped signasl
  time <- NULL

  #### Initialise segments ####
  Seg <- floor(Seg)
  pred_bound <- length(Seg) > 1
  if (pred_bound){
    if ( Seg == 1 & Seg[length(Seg)] == length(Ta) ) {
      stop('End points must be equal to 1 and to the length of the target')
    }
    len_segs <- diff(Seg)
    if (!all(len_segs > 2)) {
      stop('Segments must contain at least two points')
    }
    nSeg <- length(len_segs)
  } else{
    if (Seg > min(c(dimX[2], npT))) {
      stop('Segment length is larger than length of the signal')
    }
    if (Options[[3]]) {
      nSeg <- floor((npT - 1)/Seg)
      len_segs <- matrix(floor((npT - 1)/ nSeg), nrow = 1)
      len_segs[2, ] <- floor((dimX[2] - 1)/ nSeg)

      print('Segment lengh adjusted to the best cover the remainders')
    } else {
      nSeg = floor((npT - 1) / (Seg - 1))
      tmp_segs <- rep(Seg - 1, nSeg)
      len_segs <- matrix(c(tmp_segs, tmp_segs), nrow = 2)

      if (floor((dimX[2] -1) / (Seg - 1)) != nSeg){
        stop('For non-fixed segment lengths the target and the signal do not have the same number o fsegments. Try option 3 set to T')
      }
    }
    tmp <- (npT-1) %% len_segs[1, 1]
    if( tmp > 0) {
      len_segs[1, nSeg] <- len_segs[1, nSeg] + tmp
      if(Options[[1]]) {
        print(paste0('Segments: ', len_segs[1,1] + 1,
                     ' Points: ', nSeg -1))
      }
    } else {
      if (Options[[1]]) {
        print(paste0('Segments: ', len_segs[2, 1] + 1,
                     ' Points x: ', nSeg))
      }
    }

    tmp <- (dimX[2] - 1) %% len_segs[2, 1]
    if(tmp > 0) {
      len_segs[2, nSeg] <- len_segs[2, 1] + tmp
    }
  }

  bT <- cumsum(c(1, len_segs[1, ]))
  bP <- cumsum(c(1, len_segs[2, ]))
  Warping <- matrix(nrow = dimX[1], ncol = (nSeg+1) )

  #### Chech Slack ####
  if (length(Slack) > 1){
    if (length(Slack) <= nSeg) {
      stop('The number of slack parameters is not equal to the number of optimised segments')
    }
    stop('Multiple slacks have not been implemented yet')
  }
  Slacks_vec = seq(-Slack, Slack)

  #### Set feasible points for boundaies ####
  Bounds <- matrix(rep(1, 2*(nSeg +1)), nrow = 2)
  # Slope constrints
  offs_tmp <- Slack * seq(0, nSeg)
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
  Xdiff <- diff(X)

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

    TSeg = Ta[ seq(bT[i], bT[i + 1]) ];
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
        Xi_seg <- X[Index_Node]
        Xi_diff <- Xdiff[Index_Node]
        toreshape <- matrix(c(coeff_b, Xi_diff), nrow = 2, byrow = T)
        toreshape <- apply(toreshape, 2, prod) + Xi_seg
        Xi_seg <- matrix(toreshape, nrow = c, ncol = n_aa*dimX[1] )

        Xi_Seg_mean <- colSums(Xi_seg)/nrow(Xi_seg)
        Norm_Xi_Seg_cen <- sqrt(colSums(Xi_seg^2) - nrow(Xi_seg) * Xi_Seg_mean^2)
        CCs_Node <- ( as.numeric(TSeg_centered) %*% Xi_seg ) /
          (Norm_TSeg_cen %*% Norm_Xi_Seg_cen)
        CCs_Node <- ifelse(is.finite(CCs_Node), CCs_Node, 0)
        CCs_Node <- matrix(CCs_Node, nrow = n_aa, ncol = dimX[1])
        if(Options[[2]]){
          Cost_Fun = matrix(Table[2, nodes_tablePointer], nrow = n_aa ) + CCs_Node
        } else {
          Cost_Fun <- matrix(Table[2, nodes_tablePointer], nrow = n_aa) + CCs_Node^Options[[2]]
        }

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

  for (i in seq(1, dimX[1])) {
    Pointer <- ncol(Table)
    Warping[i, nSeg + 1] <- dimX[2]
    for (j in seq(nSeg, 1, -1)) {
      Pointer <- Table[3, Pointer]
      Warping[i, j] <- Table[1, Pointer]
    }
  }
  Xwarped <- NULL
  for (i in seq(1, nSeg)) {
    indT <- seq(bT[i], bT[i + 1])
    lenT <- bT[i + 1] - bT[i]
    for (j in seq(1, dimX[1])) {
      indX <- seq(Warping[j, i], Warping[j, i + 1])
      lenX <- Warping[j, i + 1] - Warping[j, i]
      Xwarped[indT] <- approx(x = indX - Warping[j, i] + 1,
                              y = X[indX],
                              xout = seq(0, lenT)/lenT * lenX + 1 )$y
    }
  }
  return(list(bT = bT, Warping = Warping, nSeg=nSeg,
                Xwarped = Xwarped))
  #return(Xwarped)
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

