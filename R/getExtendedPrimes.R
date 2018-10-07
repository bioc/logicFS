getExtendedPrimes <- function (primes, vec.primes)
{
  if(is.null(primes))
    return(NULL)
  a <- strsplit(primes, split = " & ")
  b <- strsplit(vec.primes, split = " & ")
  d <- numeric(length(vec.primes))
  for (i in 1:length(primes)){
    d <- d + sapply(b, function (x, y) ifelse(
      all(y %in% x) && (length(x) != length(y)), 
      1, 0), y = a[[i]])
  }
  if(sum(d) > 0)
    out <- vec.primes[d > 0]
  else
    out <- NULL
  out
}