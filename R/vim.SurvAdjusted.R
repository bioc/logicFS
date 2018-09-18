vim.SurvAdjusted <-
function (mprimes, mat.eval, inbagg, cl, neighbor, set, score)
{
  uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
  n.death <- length(uni.death.times)
  vec.improve <- numeric(ncol(mat.eval))
  oob <- which(!(1:nrow(mat.eval)) %in% inbagg)
  n.trees <- length(mprimes)
  

  vec.adjusted.primes <- unique(unlist(lapply(mprimes, getAdjustedPrimes, 
                                              mat.eval = mat.eval)))
  if(!is.null(vec.adjusted.primes)){
    for (i in 1:length(vec.adjusted.primes)){
      tmp.prime <- vec.adjusted.primes[i]
      if(!is.null(neighbor)){
        tmp.neighbors <- unlist(getNeighbor(tmp.prime, neighbor, set, mat.eval))
      } else{
        tmp.neighbors <- NULL
      }
      red.primes <- lapply(mprimes, getRedPrimes, tmp.prime = tmp.prime, 
                           tmp.neighbors = tmp.neighbors)
      mat.design <- matrix(sapply(red.primes, function(x, e = mat.eval) 
        rowSums(e[, x, drop = FALSE]) > 0), ncol = n.trees)
      score.red <- getSurvivalScore(mat.design, inbagg, oob, cl, score, 
                                        uni.death.times, n.death)
      id.change <- !(sapply(red.primes, length) == sapply(mprimes, length))
      mat.design <- cbind(apply(mat.design[, id.change, drop = FALSE], 2, 
                                function(x, a = tmp.prime) rowSums(cbind(mat.eval[, a, drop = FALSE], x)) > 0),
                          mat.design[, !id.change, drop = FALSE])
      score.full.new <- getSurvivalScore(mat.design, inbagg, oob, cl, score, 
                                     uni.death.times, n.death)
      id.primes <- which(colnames(mat.eval) %in% tmp.prime)
      vec.improve[id.primes] <-  -1 * (score.full.new - score.red)
    }
  }
  if(score != "Brier") 
    vec.improve <- -1 * vec.improve
  vec.improve
}
