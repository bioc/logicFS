getEnsemble <- function (object, vec.primes, list.primes, mat.eval, score) 
{
  # 1. Some calculations
  le.primes <- length(vec.primes)
  vec.improve <- numeric(le.primes + 1)
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
    tmp.ib <- sort(inbagg[[i]])
    oob <- which(!(1:n.obs) %in% tmp.ib)
    mat.design <- sapply(ltree[[i]]$trees,eval.logreg, data[tmp.ib, ])
    mat.new <- sapply(ltree[[i]]$trees, eval.logreg, data[oob, ])
    list.values <- getModelEnsemble(mat.design, mat.new, cl[tmp.ib], uni.death.times,
                                    n.death, score)
    mat.status[i, oob] <- list.values$status
    for (j in 1:(2^ncol(mat.design))){
      list.chf[[j]][i, ] <- list.values$chf[j, ] 
    }
  }
  results.full.model <- list(list.chf = list.chf, mat.status = mat.status) 
  vec.improve[le.primes + 1] <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                                                 n.obs, n.death, n.groups)
  # 3. Scores of reduced models
  for (h in 1:le.primes){
    mat.status <- results.full.model$mat.status
    list.chf <- results.full.model$list.chf
    prime <- vec.primes[h]
    id.models <- which(unlist(lapply(list.primes, 
                                     function (x){prime %in% unlist(x)})))
    for (i in id.models){
      tmp.ib <- inbagg[[i]]
      oob <- which(!(1:n.obs) %in% tmp.ib)
      n.trees <- length(list.primes[[i]])
      tmp.primes <- unique(unlist(list.primes[[i]]))
      n.primes <- length(tmp.primes)
      id.prime <- which(tmp.primes %in% prime)
      mat.in <- (1 - diag(n.primes))[, id.prime, drop = FALSE]
      rownames(mat.in) <- tmp.primes
      list.eval <- lapply(list.primes[[i]], function (x, e = mat.eval, i = mat.in) 
        e[, x, drop = FALSE] %*% i[x, , drop = FALSE] > 0)
      getJth <- function (x, id = NULL) x[id, ]
      mat.design <- matrix(unlist(lapply(list.eval, getJth, 
                                         id = tmp.ib)), ncol = n.trees)
      mat.new <- matrix(unlist(lapply(list.eval, getJth, 
                                      id = oob)), ncol = n.trees)
      list.values <- getModelEnsemble(mat.design, mat.new, cl[tmp.ib],  
                                      uni.death.times, n.death, score)
      mat.status[i, oob] <- as.numeric(list.values$status)
      for (j in 1:(2^ncol(mat.design))){
        list.chf[[j]][i, ] <- list.values$chf[j, ] 
      }
    }
    vec.improve[h] <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                                       n.obs, n.death, n.groups)
  }
  vec.improve[-(le.primes + 1)] <- 
    (-1) * (vec.improve[-(le.primes + 1)] - vec.improve[le.primes + 1])
  vec.improve 
}