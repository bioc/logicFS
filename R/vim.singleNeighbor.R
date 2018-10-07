vim.singleNeighbor <- function (mprimes, mat.eval, cl, 
                                neighbor, set, useN = TRUE) 
{ 
  # For b-th iteration
  vec.improve <- numeric(ncol(mat.eval))
  # 1. Find all primes P_a^b of the b-th logic regression model
  vec.mprimes <- unique(unlist(mprimes))
  # 2. Identify neighbor interactions of primes found in 1.
  neighborint <- getNeighbor(vec.mprimes, neighbor, set, colnames(mat.eval))
  if (any(sapply(neighborint, is.null))){
    vec.model <- rowSums(mat.eval[, vec.mprimes, drop = FALSE]) > 0
    sfm <- sum(vec.model == cl)
  }
  if(any(vec.mprimes %in% unlist(neighborint)))
    neighborint <- check.neighborint(neighborint)
  # 3. For each set of P_a^b and its neighbor interactions 
  for (h in 1:length(neighborint)){
    setneighbor <- c(names(neighborint)[h], unlist(neighborint[h]))
    tmp.mprimes <- unlist(lapply(mprimes, function (x, b = setneighbor) 
      x[!(x %in% b)]))
    # b) Calculate score of reduced model
    vec.model <- rowSums(mat.eval[, tmp.mprimes, drop = FALSE]) > 0
    score.red <- sum(vec.model == cl)
    
    # c) Add each prime in setneighbor seperately to logic model and calculate 
    # score of new (full) model 
    if (length(setneighbor) > 1){
      for (j in 1:length(setneighbor)){
        new.mprimes <- c(tmp.mprimes, setneighbor[j])
        vec.model <- rowSums(mat.eval[, new.mprimes, drop = FALSE]) > 0
        score.full <- sum(vec.model == cl)
        # d) Calculate and save improvement
        id.primes <- which(colnames(mat.eval) %in% setneighbor[j])
        vec.improve[id.primes] <- score.full - score.red
      }
    } else {
      # d) Calculate and save improvement
      id.primes <- which(colnames(mat.eval) %in% setneighbor)
      vec.improve[id.primes] <- sfm - score.red
    }
  }
  if (!useN) 
    vec.improve <- vec.improve/length(cl)
  vec.improve
}