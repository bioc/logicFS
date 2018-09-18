getConc <- function (oob.eche, cl)
  {
    y <- cl
    p <- ncol(y)
    time <- y[, p - 1]
    status <- y[, p]
    x <- oob.eche
    n <- length(time)
    ord <- order(time, -status)
    time <- time[ord]
    status <- status[ord]
    x <- x[ord]
    wh <- which(status == 1)
    total <- concordant <- 0
    for (i in wh){
      if (i < n){
        for (j in ((i + 1):n)){
          if (time[j] > time[i]){
            total <- total + 1
            if (x[j] < x[i]) 
              concordant <- concordant + 1
            if (x[j] == x[i]) 
              concordant <- concordant + 0.5
          } else if (status[j] == 1){
            total <- total + 1
            if (x[j] == x[i]){
              concordant <- concordant + 1
            } else concordant <- concordant + 0.5
          } else {
            total <- total + 1
            if (x[j] < x[i]){
              concordant <- concordant + 1
            } else concordant <- concordant + 0.5
          }
        }
      }
    }
    out <- concordant / total
    out
  }
