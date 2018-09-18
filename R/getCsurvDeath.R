getCsurvDeath <-
function (csurv, cl, uni.death.times)
  {
    csurv.death <- numeric(length(uni.death.times)) + 1
    cl.death <- cl[which(cl[, 2] == 1), 1]
    id.death.times <- which(cl[, 2] == 1)
    id.uni.death.times <- id.death.times[which(c(1, diff(cl.death)) != 0)]
    cl.uni.death <- cl[id.uni.death.times, 1]
    csurv.death[which(round(uni.death.times, 8) %in% round(cl.uni.death, 8))] <- csurv[id.uni.death.times]
    id.oob <- which(!(round(uni.death.times, 8) %in% round(cl.uni.death, 8)))
    if (id.oob[1] == 1) id.oob <- id.oob[-1]
    for (i in id.oob){
      csurv.death[i] <- csurv.death[i - 1]
    }
    return(csurv.death)
  }
