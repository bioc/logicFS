getBrier <- function (mat.oob.eche, cl, uni.death.times)
  {
    if (nrow(mat.oob.eche) != nrow(cl)){
      stop("nrow(mat.oob.eche) != nrow(cl)")
    }
    
    n.death <- length(uni.death.times)
    time <- cl[, 1]
    ot <- order(time)
    cens <- cl[ot, 2]
    time <- time[ot]
    cl <- Surv(time, cens)
    mat.oob.eche <- mat.oob.eche[ot, ]
    cl.cens <- cl
    cl.cens[, 2] <- !cl[, 2]
    survs <- mat.oob.eche
    
    
    
    hatcdist <- survfit(cl.cens ~ 1)
    csurv <- getCsurv(hatcdist, cl)
    csurv[csurv == 0] <- Inf
    
    csurv.death <- getCsurvDeath(csurv, cl, uni.death.times)
    
    bsc <- rep(0, n.death)
    
    for (j in 1:n.death){
      help1 <- as.integer(time <= uni.death.times[j] & cens == 1)
      help2 <- as.integer(time > uni.death.times[j])
      bsc[j] <- mean((0 - survs[, j])^2 * help1 * (1/csurv) + 
                       (1 - survs[, j])^2 * help2 * (1/csurv.death[j]))
    }
    idx <- 2:n.death
    RET <- diff(uni.death.times) %*% ((bsc[idx - 1] + bsc[idx])/2)
    RET <- RET/diff(range(time))
    
    return(RET)
  }
