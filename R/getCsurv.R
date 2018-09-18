getCsurv <-
function (hatcdist, cl)
  {
    surv <- hatcdist$surv
    time <- hatcdist$time
    csurv <- numeric(nrow(cl))
    for (i in 1:nrow(cl)){
      idtime <- which(round(time, 8) %in% round(cl[i, 1], 8))
      if (length(idtime) == 0){
        k <- 7
        idtime <- which(round(time, k) %in% round(cl[i, 1], k))
        while ((length(idtime) == 0) && (k > 1)){
          k <- k - 1
          idtime <- which(round(time, k) %in% round(cl[i, 1], k))
        }
      }
      csurv[i] <- surv[idtime[1]]
    }
    return(csurv)
  }
