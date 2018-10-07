getDPO <- function (oob.eche, cl) 
{
  y <- cl
  p <- ncol(y)
  time <- y[, p - 1] / sqrt(sum(y[, p - 1]^2))
  status <- y[, p]
  x <- oob.eche / sqrt(sum(oob.eche^2))
  if (length(unique(time)) == 1 | length(unique(x)) == 1)
    return(0.5)
  min_time_diff <- min(diff(sort(time))[diff(sort(time)) > 0]) / 2
  min_oob_diff <- min(diff(sort(x))[diff(sort(x)) > 0]) / 2
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
          total <- total + sqrt(max(abs(x[i] - x[j]), min_oob_diff) * (time[j] - time[i]))
          if (x[j] < x[i]) 
            concordant <- concordant + sqrt((x[i] - x[j]) * (time[j] - time[i]))
          if (x[j] == x[i]) 
            concordant <- concordant + sqrt(min_oob_diff * (time[j] - time[i])) / 2
        } else if (status[j] == 1){
          total <- total + sqrt(max(abs(x[i] - x[j]), min_oob_diff) * min_time_diff)
          if (x[j] == x[i]){
            concordant <- concordant + sqrt(min_oob_diff * min_time_diff)
          } else concordant <- concordant + sqrt(abs(x[i] - x[j]) * min_time_diff) / 2
        } else {
          total <- total + sqrt(max(abs(x[i] - x[j]), min_oob_diff) * min_time_diff)
          if (x[j] < x[i]){
            concordant <- concordant + sqrt((x[i] - x[j]) * min_time_diff)
          } else concordant <- concordant + sqrt(max(abs(x[i] - x[j]), min_oob_diff) * min_time_diff) / 2
        }
      }
    }
  }
  concordant / total
}