getSurvs <-
function (mat.oob.eche, cl)
  {
    survs <- matrix(1, nrow = nrow(cl), ncol = nrow(cl))
    cl.death <- cl[cl[, 2] == 1,]
    id.uni.death.times <- which(cl[, 2] == 1)
    id.uni.death.times <- id.uni.death.times[which(c(1, diff(cl.death)) != 0)]
    survs[, id.uni.death.times] <- mat.oob.eche
    id.oob <- which(!(cl[, 2] == 1 & c(1,diff(cl[, 1])) != 0))
    if (id.oob[1] == 1) id.oob <- id.oob[-1]
    for (i in id.oob){
      survs[, i] <- survs[, i - 1]
    }
    return(survs)
  }
