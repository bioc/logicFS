getEnsembleNeighbor <- function (object, vec.primes, list.primes, 
                                 mat.eval, score, neighbor, set)
{
  # 1. Some calculations
  le.primes <- length(vec.primes)
  vec.improve <- numeric(le.primes)
  inbagg <- object$inbagg
  ltree <- object$logreg.model
  data <- object$data
  cl <- object$cl
  uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
  n.death <- length(uni.death.times)
  n.obs <- nrow(data)
  n.var <- ncol(data)
  n.trees <- object$ntrees
  n.groups <- 2^(n.trees)
  B <- length(ltree)
  mat.status <- matrix(0, nrow = B, ncol = n.obs)
  mat.chf <- matrix(0, nrow = B, ncol = n.death)
  list.chf <- vector("list", n.groups)
  for (j in 1:n.groups) list.chf[[j]] <- mat.chf
  # 2. Score of full model
  for (i in 1:B){
    tmp.ib <- inbagg[[i]]
    oob <- which(!(1:n.obs) %in% tmp.ib)
    mat.design <- sapply(ltree[[i]]$trees, eval.logreg, 
                         data[tmp.ib, ])
    mat.new <- sapply(ltree[[i]]$trees, eval.logreg, 
                      data[oob, ])
    
    list.values <- getModelEnsemble(mat.design, mat.new, 
                                    cl[tmp.ib], uni.death.times, n.death, 
                                    score)
    mat.status[i, oob] <- list.values$status
    for (j in 1:(2^ncol(mat.design))){
      list.chf[[j]][i, ] <- list.values$chf[j, ] 
    }
  }
  sfm <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                          n.obs, n.death, n.groups)
  results.full.model <- list(list.chf = list.chf, 
                             mat.status = mat.status) 
  # Scores of reduced models
  for (h in 1:le.primes){
    # In h-th step the score of prime h is calculated
    # First we calculate the score of the reduced model
    prime <- vec.primes[h]
    setneighbor <- getNeighbor(prime, neighbor, set, vec.primes)
    setneighbor <- c(prime, unlist(setneighbor))
    mat.status <- results.full.model$mat.status
    list.chf <- results.full.model$list.chf
    if (length(setneighbor) > 1){
      mat.statusFull <- results.full.model$mat.status
      list.chfFull <- results.full.model$list.chf
    } else{
      mat.statusFull <- list.chfFull <- NULL
    }
    id.models <- which(unlist(lapply(list.primes, 
                                     function (x, y = setneighbor){any(y %in% unlist(x))})))
    for (i in id.models){
      tmp.ib <- inbagg[[i]]
      oob <- which(!(1:n.obs) %in% tmp.ib)
      # a) Remove prime and its neighbor interactions from logic model
      tmp.mprimes <- lapply(list.primes[[i]], 
                            function (x, b = setneighbor) x[!(x %in% b)])
      # b) Calculate cumulative hazard or survival function of reduced model
      mat.model <- matrix(unlist(lapply(tmp.mprimes, function (x, e = mat.eval) 
        rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(list.primes[[i]])) 
      list.values <- getModelEnsemble(mat.model[tmp.ib, , drop = FALSE], 
                                      mat.model[oob, , drop = FALSE], cl[tmp.ib], 
                                      uni.death.times, n.death, score)
      mat.status[i, oob] <- as.numeric(list.values$status)
      for (j in 1:(2^ncol(mat.model))){
        list.chf[[j]][i, ] <- list.values$chf[j, ] 
      }
      if (length(setneighbor) > 1){
        id.change <- sapply(tmp.mprimes, length) != sapply(list.primes[[i]], length)
        new.mprimes <- tmp.mprimes
        new.mprimes[id.change] <- lapply(tmp.mprimes[id.change], 
                                         function (x) append(x, prime))
        mat.model <- matrix(unlist(lapply(new.mprimes, function (x, e = mat.eval) 
          rowSums(e[, x, drop = FALSE]) > 0)), ncol = length(list.primes[[i]]))
        list.values <- getModelEnsemble(mat.model[tmp.ib, , drop = FALSE], 
                                        mat.model[oob, , drop = FALSE], cl[tmp.ib], 
                                        uni.death.times, n.death, score)
        mat.statusFull[i, oob] <- as.numeric(list.values$status)
        for (j in 1:(2^ncol(mat.design))){
          list.chfFull[[j]][i, ] <- list.values$chf[j, ] 
        }
      }
    }
    score.red <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                                  n.obs, n.death, n.groups)
    # Now we calculate the score of the new full model (which is
    # equal to the score of the full model, if prime h has no neighbors)
    if (length(setneighbor) > 1){
      score.full <- getScoreEnsemble(list.chfFull, mat.statusFull, cl, 
                                     score, n.obs, n.death, n.groups)
      vec.improve[h] <- score.red - score.full
    } else {
      vec.improve[h] <- score.red - sfm
    }
  }
  if (score == "Conc")
    sfm <- 1 - sfm
  vec.improve <- c((-1) * vec.improve, sfm) 
  vec.improve
}