getProp <- function(list.primes, vec.primes, mat.eval, set, neighbor, adjusted)
{
  B <- length(list.primes)
  le.primes <- length(vec.primes)
  prop <- numeric(le.primes)
  names(prop) <- vec.primes
  for (h in 1:le.primes){
    prime <- vec.primes[h]
    if (!is.null(neighbor)){
      setneighbor <- getNeighbor(prime, neighbor, set, vec.primes)
      setneighbor <- c(prime, unlist(setneighbor))
    } else{
      setneighbor <- prime
    }
    if (adjusted){
      setextended <- unlist(lapply(setneighbor, getExtendedPrimes, 
                                   vec.primes = vec.primes))
    } else{
      setextended <- NULL
    }
    setprime <- unique(c(setneighbor, setextended))
    id.models <- which(unlist(lapply(list.primes, 
                                     function (x, y = setprime){any(y %in% unlist(x))})))
    prop[h] <- length(id.models) / B
  }
  prop
}