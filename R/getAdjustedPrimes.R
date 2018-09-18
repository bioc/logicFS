getAdjustedPrimes <-
function(primes, mat.eval)
{
  a <- strsplit(colnames(mat.eval), split = " & ")
  b <- strsplit(primes, split = " & ")
  d <- numeric(length(a))
  for(i in 1:length(b)){
    d <- d + sapply(a, function(x, y) ifelse(all(x %in% y), TRUE, FALSE), y = b[[i]])
  }
  if(sum(d) > length(primes)){
    out <- colnames(mat.eval)[d > 0]
    out <- out[!(out %in% primes)]
  }else out <- NULL
  unique(out)
}
