vim.singleAdjusted <-
function (primes, mat.eval, cl, neighbor, set, useN = TRUE)
{
  vec.improve <- numeric(ncol(mat.eval))
  vec.adjusted.primes <- getAdjustedPrimes(primes, mat.eval)
  if(!is.null(vec.adjusted.primes)){
    for (i in 1:length(vec.adjusted.primes)){
      tmp.prime <- vec.adjusted.primes[i]
      if(!is.null(neighbor)){
        tmp.neighbors <- unlist(getNeighbor(tmp.prime, neighbor, set, mat.eval))
      } else{
        tmp.neighbors <- NULL
      }
      red.primes <- getRedPrimes(primes, tmp.prime, tmp.neighbors)
      id.primes <- colnames(mat.eval) %in% red.primes
      vec.design.red <- rowSums(mat.eval[, id.primes, drop = FALSE]) > 0 
      score.red <- sum(vec.design.red == cl)
      id.primes <- colnames(mat.eval) %in% tmp.prime
      preds.new <- rowSums(cbind(mat.eval[, id.primes, drop = FALSE], vec.design.red)) > 0 
      score.full.new <- sum(preds.new == cl)
      vec.improve[id.primes] <- score.full.new - score.red
    }
  }
  if (!useN) 
    vec.improve <- vec.improve/length(cl)
  vec.improve
}
