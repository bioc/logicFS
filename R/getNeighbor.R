getNeighbor <- function (primes, neighbor, set, vec.primes)
{
  if (is.null(primes))
    return(NULL)
  if (is.null(set) | is.null(neighbor))
    return(NULL)
  list.neighbor.primes <- vector("list", length(primes))
  list.snp <- strsplit(primes, " & ")
  for (i in 1:length(primes)){
    vec.changed.primes <- getNeighborhoodPrime(list.snp[[i]], neighbor, set)
    if (any(vec.changed.primes %in% vec.primes)){
      a <- vec.changed.primes[vec.changed.primes %in% vec.primes] 
      if (any(a %in% primes[i]))
        a <- a[-which(a %in% primes[i])]
      if (length(a) == 0)
        next
      list.neighbor.primes[[i]] <- unique(a)
    }
  }
  names(list.neighbor.primes) <- primes
  list.neighbor.primes
}