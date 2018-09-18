getRedPrimes <-
function(primes, tmp.prime, tmp.neighbors)
{
  a <- c(tmp.prime, tmp.neighbors)
  b <- strsplit(a, split = " & ")
  d <- strsplit(primes, split = " & ")
  f <- numeric(length(primes))
  for(i in 1:length(b)){
    f <- f + sapply(d, function(x, y) all(y %in% x), y = b[[i]])
  }
  primes[f == 0]
}
