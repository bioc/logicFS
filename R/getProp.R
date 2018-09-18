getProp <-
function(vec.primes, list.primes, neighbor, set, mat.eval, adjusted)
{
  prop <- numeric(length(vec.primes))
  for(j in 1:length(vec.primes)){
    out <- numeric(length(list.primes))
    prime <- vec.primes[j]
    tmp.nb.primes <- unlist(getNeighbor(prime, neighbor = neighbor, 
                                        set = set, mat.eval = mat.eval))
    p <- strsplit(c(prime, tmp.nb.primes), " & ")
    fxyz <- function(x, y, z)
    {
      x <- strsplit(x, " & ")
      if(z)
        any(sapply(y, function(g) any(sapply(x, function(a, b) ifelse(all(b %in% a), 
                                                                      TRUE, FALSE), b = g))))
      else any(sapply(y, function(g) any(sapply(x, function(a, b) 
        ifelse(all(b %in% a) & (length(b) == length(a)), TRUE, FALSE), b = g))))
    }
    for(i in 1:length(list.primes)){
      if(any(sapply(list.primes[[i]], fxyz, y = p, z = adjusted)))
        out[i] <- 1
    }
    prop[j] <- sum(out) / length(list.primes)
  }
  prop
}
