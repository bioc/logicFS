vim.singleNeighbor <-
function (mprimes, mat.eval, cl, neighbor, set, useN = TRUE) 
{ 
  vec.improve <- numeric(ncol(mat.eval))
  vec.mprimes <- unique(unlist(mprimes))
  for(h in 1:length(vec.mprimes)){
    prime <- vec.mprimes[h]
    tmp.nb.primes <- unlist(getNeighbor(prime, neighbor = neighbor, 
                                        set = set, mat.eval = mat.eval))
    if(!is.null(tmp.nb.primes)){
      for(j in 1:length(tmp.nb.primes)){
        Nj <- unlist(getNeighbor(tmp.nb.primes[j], neighbor = neighbor, 
                                 set = set, mat.eval = mat.eval))
        tmp.mprimes <- mprimes[!(mprimes %in% c(tmp.nb.primes[j], Nj))]
        vec.model <- rowSums(mat.eval[, tmp.mprimes, drop = FALSE]) > 0
        score.red <- sum(vec.model == cl)
        tmp.mprimes <- c(tmp.mprimes, tmp.nb.primes[j])
        vec.model <- rowSums(mat.eval[, tmp.mprimes, drop = FALSE]) > 0
        score.full <- sum(vec.model == cl)
        id.primes <- which(colnames(mat.eval) %in% tmp.nb.primes[j])
        vec.improve[id.primes] <- score.full - score.red
      }
    }
  }
  if (!useN) 
    vec.improve <- vec.improve/length(cl)
  vec.improve
}
