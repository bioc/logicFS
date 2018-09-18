getNeighbor <-
function (primes, neighbor, set, mat.eval)
{
  if(is.null(set) | is.null(neighbor))
    return(NULL)
  list.neighbor.primes <- vector("list", length(primes))
  list.snp <- strsplit(primes, " & ")
  for(i in 1:length(primes)){
    vec.changed.primes <- getNeighborhoodPrime(list.snp[[i]], neighbor, set)
    if(any(vec.changed.primes %in% colnames(mat.eval))){
      a <- vec.changed.primes[vec.changed.primes %in% colnames(mat.eval)] 
      if(any(a %in% primes[i]))
        a <- a[-which(a %in% primes[i])]
      if(length(a) == 0)
        a <- NULL
      list.neighbor.primes[[i]] <- a
    }
  }
  list.neighbor.primes
}
