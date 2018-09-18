getVecGroup <-
function (x)
  {
    out <- numeric(x)
    for (l in 0:(x-1)){out[l + 1] <- 2^l} 
    out
  }
