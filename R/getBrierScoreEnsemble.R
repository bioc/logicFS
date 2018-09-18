getBrierScoreEnsemble <- function (mat.oob.eche, cl)
  {
    if (nrow(mat.oob.eche) != nrow(cl)){
      stop("nrow(mat.oob.eche) != nrow(cl)")
    }
    
    n.obs <- nrow(cl)
    #n.obs <- length(unique(cl[,1]))
    time <- cl[, 1]
    ot <- order(time)
    cens <- cl[ot, 2]
    time <- time[ot]
    cl <- Surv(time, cens)
    mat.oob.eche <- mat.oob.eche[ot, ]
    cl.cens <- cl
    cl.cens[, 2] <- !cl[, 2]
    survs <- getSurvs(mat.oob.eche, cl)
    
    
    
    hatcdist <- survfit(cl.cens ~ 1)
    csurv <- getCsurv(hatcdist, cl)
    csurv[csurv == 0] <- Inf
    
    bsc <- rep(0, n.obs)
    
    for (j in 1:n.obs){
      help1 <- as.integer(time <= time[j] & cens == 1)
      help2 <- as.integer(time > time[j])
      bsc[j] <- mean((0 - survs[, j])^2 * help1 * (1/csurv) + 
                       (1 - survs[, j])^2 * help2 * (1/csurv[j]))
    }
    idx <- 2:n.obs
    RET <- diff(time) %*% ((bsc[idx - 1] + bsc[idx])/2)
    RET <- RET/diff(range(time))
    return(RET)
  }
