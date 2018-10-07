vim.lmNeighbor <- function (mprimes, mat.eval, inbagg, cl, neighbor, set)
{
  # For b-th iteration
  vec.improve <- numeric(ncol(mat.eval)) 
  oob <- which(!(1:nrow(mat.eval)) %in% inbagg)
  # 1. Find all primes P_a^b of the b-th logic regression model
  vec.mprimes <- unique(unlist(mprimes))
  # 2. Identify neighbor interactions of primes found in 1.
  neighborint <- getNeighbor(vec.mprimes, neighbor, set, colnames(mat.eval))
  if (any(sapply(neighborint, is.null))){
    mat.model <- matrix(unlist(lapply(mprimes, function (x, e = mat.eval) 
      rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(mprimes)) 
    sfm <- getLmScore(cl, mat.model, inbagg, oob)
  }
  if(any(vec.mprimes %in% unlist(neighborint)))
    neighborint <- check.neighborint(neighborint)
  # 3. For each set of P_a^b and its neighbor interactions 
  for (h in 1:length(neighborint)){
    # a) Remove P_a^b and its neighbor interactions from logic model
    setneighbor <- c(names(neighborint)[h], unlist(neighborint[h]))
    tmp.mprimes <- lapply(mprimes, function (x, b = setneighbor) x[!(x %in% b)])
    # b) Calculate score of reduced model
    mat.model <- matrix(unlist(lapply(tmp.mprimes, function (x, e = mat.eval) 
      rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(mprimes)) 
    score.red <- getLmScore(cl, mat.model, inbagg, oob)
    # c) Add each prime in setneighbor seperately to logic model and calculate score 
    if (length(setneighbor) > 1){
      id.change <- sapply(tmp.mprimes, length) != sapply(mprimes, length)
      for (j in 1:length(setneighbor)){
        new.mprimes <- tmp.mprimes
        new.mprimes[id.change] <- lapply(tmp.mprimes[id.change], function (x) append(x, setneighbor[j]))
        mat.model <- matrix(unlist(lapply(new.mprimes, function (x, e = mat.eval) 
          rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(mprimes))
        score.full <- getLmScore(cl, mat.model, inbagg, oob)
        # d) Calculate and save improvement
        id.primes <- which(colnames(mat.eval) %in% setneighbor[j])
        vec.improve[id.primes] <- score.red - score.full
      }
    } else {
      # d) Calculate and save improvement
      id.primes <- which(colnames(mat.eval) %in% setneighbor)
      vec.improve[id.primes] <- score.red - sfm
    }
  }
  vec.improve
}