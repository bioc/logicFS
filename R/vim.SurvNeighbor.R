vim.SurvNeighbor <-
function (mprimes, mat.eval, inbagg, cl, neighbor, set, score)
{
  uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
  n.death <- length(uni.death.times)
  vec.improve <- numeric(ncol(mat.eval))
  oob <- which(!(1:nrow(mat.eval)) %in% inbagg)
  vec.mprimes <- unique(unlist(mprimes))
  for(h in 1:length(vec.mprimes)){
    prime <- vec.mprimes[h]
    tmp.nb.primes <- unlist(getNeighbor(prime, neighbor = neighbor, 
                                        set = set, mat.eval = mat.eval))
    if(!is.null(tmp.nb.primes)){
      for(j in 1:length(tmp.nb.primes)){
        Nj <- unlist(getNeighbor(tmp.nb.primes[j], neighbor = neighbor, 
                                 set = set, mat.eval = mat.eval))
        tmp.mprimes <- lapply(mprimes, function(x, b = c(tmp.nb.primes[j], Nj)) x[!(x %in% b)])
        mat.model <- mat.design <- matrix(unlist(lapply(tmp.mprimes, function(x, e = mat.eval) 
          rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(mprimes))
        score.red <- getSurvivalScore(mat.model, inbagg, oob, cl, score, 
                                       uni.death.times, n.death)
         
        id.change <- sapply(tmp.mprimes, length) != sapply(mprimes, length)
        mat.model <- cbind(apply(mat.design[, id.change, drop = FALSE], 2, 
                                 function(x, a = tmp.nb.primes[j]) rowSums(cbind(mat.eval[, a, drop = FALSE], x)) > 0),
                           mat.design[, !id.change, drop = FALSE])
        score.full <- getSurvivalScore(mat.model, inbagg, oob, cl, score, 
                                        uni.death.times, n.death)
        id.primes <- which(colnames(mat.eval) %in% tmp.nb.primes[j])
        vec.improve[id.primes] <- -(score.full - score.red)
      }
    }
  }
  vec.improve
  if(score != "Brier") 
    vec.improve <- -1 * vec.improve
  vec.improve
}
