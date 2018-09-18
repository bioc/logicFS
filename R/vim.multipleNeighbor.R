vim.multipleNeighbor <-
function (mprimes, mat.eval, inbagg, cl, prob.case = 0.5, neighbor, set, useN = TRUE)
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
          cl.train <- cl[-oob]
          coef.null <- log(sum(cl.train == 1)/sum(cl.train == 0))
          score.red <- ifelse(exp(coef.null) / (1- exp(coef.null)) > prob.case, 
                              sum(cl[oob]), sum(1 - cl[oob])) 
        } else{
          mat.model <- data.frame(cl = cl, mat.model)
          glm.out <- glm(cl ~ ., data = mat.model[inbagg, ], family = "binomial")
          preds <- predict(glm.out, mat.model[oob, ], type = "response") > 
            prob.case
          score.red <- sum(preds == cl[oob])
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
          glm.out <- glm(cl ~ ., data = mat.model[inbagg, ], family = "binomial")
          preds <- predict(glm.out, mat.model[oob, ], type = "response") > 
            prob.case
          score.full <- sum(preds == cl[oob])
        }
        id.primes <- which(colnames(mat.eval) %in% tmp.nb.primes[j])
        vec.improve[id.primes] <- score.full - score.red
      }
    }
  }
  if (!useN) 
    vec.improve <- vec.improve/length(oob)
  vec.improve
}
