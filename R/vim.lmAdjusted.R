vim.lmAdjusted <-
function (mprimes, mat.eval, inbagg, cl, prob.case = 0.5, neighbor, set, useN = TRUE)
{
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
      mat.design <- mat.model <- matrix(sapply(red.primes, function(x, e = mat.eval) 
        rowSums(e[, x, drop = FALSE]) > 0), ncol = n.trees)
      if (any(colSums(mat.design) == 0)) 
        mat.design <- mat.design[, colSums(mat.design) > 0, drop = FALSE]
      if(ncol(mat.design) == 0){
        beta0 <- mean(cl[inbagg])
        rss.null <- mean((beta0 - cl[oob])^2)
        score.red <- log2(rss.null)
      } else{mat.design <- data.frame(cl = cl, mat.design)
      lm.out <- lm(cl ~ ., data = mat.design[inbagg, ])
      preds <- predict(lm.out, mat.design[oob, ])
      score.red <- log2(mean((preds - cl[oob])^2))
      }
      id.change <- !(sapply(red.primes, length) == sapply(mprimes, length))
      mat.design <- cbind(apply(mat.model[, id.change, drop = FALSE], 2, 
                                function(x, a = tmp.prime) rowSums(cbind(mat.eval[, a, drop = FALSE], x)) > 0),
                          mat.model[, !id.change, drop = FALSE])
      mat.design <- data.frame(cl = cl, mat.design)
      lm.out <- lm(cl ~ ., data = mat.design[inbagg, ])
      preds <- predict(lm.out, mat.design[oob, ])
      score.full.new <- log2(mean((preds - cl[oob])^2))
      id.primes <- colnames(mat.eval) %in% tmp.prime
      vec.improve[id.primes] <- score.full.new - score.red
    }
  }
  vec.improve
}
