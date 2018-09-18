vim.lmNeighbor <-
function (mprimes, mat.eval, inbagg, cl, neighbor, set) 
{
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
        if (any(colSums(mat.model) == 0)) 
          mat.model <- mat.model[, colSums(mat.model) > 0, drop = FALSE]
        if(ncol(mat.model) == 0){
          beta0 <- mean(cl[inbagg])
          rss.null <- mean((beta0 - cl[oob])^2)
          score.red <- log2(rss.null)
        } else{
          mat.model <- data.frame(cl = cl, mat.model)
          lm.out <- lm(cl ~ ., data = mat.model[inbagg, ])
          preds <- predict(lm.out, mat.model[oob, ])
          score.red <- log2(mean((preds - cl[oob])^2))
        }
        id.change <- sapply(tmp.mprimes, length) != sapply(mprimes, length)
        mat.model <- cbind(apply(mat.design[, id.change, drop = FALSE], 2, 
                                 function(x, a = tmp.nb.primes[j]) rowSums(cbind(mat.eval[, a, drop = FALSE], x)) > 0),
                           mat.design[, !id.change, drop = FALSE])
        if (any(colSums(mat.model) == 0)) 
          mat.model <- mat.model[, colSums(mat.model) > 0]
       if(ncol(mat.model) == 0){
          score.full <- score.red
        } else{
          mat.model <- data.frame(cl = cl, mat.model)
          lm.out <- lm(cl ~ ., data = mat.model[inbagg, ])
          preds <- predict(lm.out, mat.model[oob, ])
          score.full <- log2(mean((preds - cl[oob])^2))
        }
        id.primes <- which(colnames(mat.eval) %in% tmp.nb.primes[j])
        vec.improve[id.primes] <- score.full - score.red
      }
    }
  }
  vec.improve
}
