vim.lmAdjusted <- function (mprimes, mat.eval, inbagg, cl, neighbor, set)
{
  # For b-th iteration
  # 1. mprimes includes all primes P_a^b of the b-th logic regression model
  vec.improve <- numeric(ncol(mat.eval))
  oob <- which(!(1:nrow(mat.eval)) %in% inbagg)
  n.trees <- length(mprimes)
  primes <- unique(unlist(mprimes))
  # 2. For each prime P_a found in 1., identify neighbors of P_a, composing primes
  #    of P_a and neighbors of composing primes of P_a. imp.primes includes all 
  #    primes, for wich an improvement is calculated in iteration b
  comp.primes <- unique(getComposingPrimes(primes, colnames(mat.eval)))
  neighborprimes <- unique(unlist(getNeighbor(primes, neighbor, set, 
                                              colnames(mat.eval))))
  neighborcomp.primes <- unique(unlist(getNeighbor(comp.primes, neighbor, 
                                                   set, colnames(mat.eval))))
  imp.primes <- unique(c(primes, neighborprimes, comp.primes, neighborcomp.primes))
  # 3. For each prime i in imp.primes
  for (i in 1:length(imp.primes)){
    tmp.prime <- imp.primes[i]
    # a) Identify neighbors of prime i, extended interactions of prime i 
    #    and extended interactions of neighbor interactions of prime i,
    #    that are part of the logic model.
    neighbortmp.primes <- unique(unlist(getNeighbor(tmp.prime, neighbor, 
                                                    set, primes)))
    ext.tmp.primes <- getExtendedPrimes(tmp.prime, primes)
    ext.neighbortmp.primes <- getExtendedPrimes(neighbortmp.primes, primes)
    setprime <- unique(c(tmp.prime, ext.tmp.primes, neighbortmp.primes, 
                         ext.neighbortmp.primes))
    # b) Remove all primes in setprime from the logic model 
    #    and calculate the score of the reduced model.
    red.primes <- lapply(mprimes, function (x, b = setprime) x[!(x %in% b)])
    mat.model <- matrix(unlist(lapply(red.primes, function (x, e = mat.eval) 
      rowSums(e[, x, drop = FALSE]) > 0)), ncol = n.trees) 
    score.red <- getLmScore(cl, mat.model, inbagg, oob)
    # c) Add prime i to the logic model and calculate the score of the new (full) model
    id.change <- sapply(red.primes, length) != sapply(mprimes, length)
    new.mprimes <- red.primes
    new.mprimes[id.change] <- lapply(red.primes[id.change], function(x) append(x, tmp.prime))
    mat.model <- matrix(unlist(lapply(new.mprimes, function(x, e = mat.eval) 
      rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(mprimes))
    score.full <- getLmScore(cl, mat.model, inbagg, oob)
    # d) Calculate and save improvement
    id.primes <- which(colnames(mat.eval) %in% tmp.prime)
    vec.improve[id.primes] <- score.red - score.full
  }
  vec.improve
}