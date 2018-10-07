getSetEnsemble <- function(object, set, score)
{
  n.set <- length(set)
  vec.improve <- numeric(n.set + 1)
  inbagg <- object$inbagg
  ltree <- object$logreg.model
  data <- object$data
  cl <- object$cl
  uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
  n.death <- length(uni.death.times)
  n.obs <- nrow(data)
  n.var <- ncol(data)
  n.groups <- 2^(object$ntrees)
  B <- length(ltree)
  mat.status <- matrix(0, nrow = B, ncol = n.obs)
  mat.chf <- matrix(0, nrow = B, ncol = n.death)
  list.chf <- vector("list", n.groups)
  for (j in 1:n.groups) list.chf[[j]] <- mat.chf
  # 2. Score of full model
  for (i in 1:B){
    tmp.ib <- sort(inbagg[[i]])
    oob <- which(!(1:n.obs) %in% tmp.ib)
    mat.design <- sapply(ltree[[i]]$trees, 
                         eval.logreg, data[tmp.ib, ])
    mat.new <- sapply(ltree[[i]]$trees, 
                      eval.logreg, data[oob, ])
    list.values <- getModelEnsemble(mat.design, mat.new, cl[tmp.ib], 
                                    uni.death.times, n.death, score)
    mat.status[i, oob] <- list.values$status
    for (j in 1:(2^ncol(mat.design))){
      list.chf[[j]][i, ] <- list.values$chf[j, ] 
    }
  }
  results.full.model <- list(list.chf = list.chf, mat.status = mat.status) 
  vec.improve[n.set + 1] <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                                             n.obs, n.death, n.groups)
  # 3. Scores of reduced models
  data <- cbind(data, 1, 0)
  for (h in 1:n.set){
    mat.status <- results.full.model$mat.status
    list.chf <- results.full.model$list.chf
    id.models <- which(unlist(lapply(ltree, function (x)
    {any(set[[h]] %in% unique(x$trees[[1]]$trees$knot))})))
    for (i in id.models){
      tmp.ib <- inbagg[[i]]
      oob <- which(!(1:n.obs) %in% tmp.ib)
      newtree <- lapply(ltree[[i]]$trees, getNewTree, set[[h]], n.var)
      newtree <- lapply(newtree, checkNewTree, n.var)
      ids <- !unlist(lapply(newtree, is.null))
      if (sum(ids) == 0) {
        mat.model <- matrix(1, nrow = nrow(data), ncol = 1)
      } else {
        newtree <- newtree[ids]
        mat.model <- sapply(newtree, eval.logreg, data)
      }
      list.values <- getModelEnsemble(mat.model[tmp.ib, , drop = F], 
                                      mat.model[oob, , drop = F], cl[tmp.ib], 
                                      uni.death.times, n.death, score)
      mat.status[i, oob] <- as.numeric(list.values$status)
      for (j in 1:(2^ncol(mat.design))){
        list.chf[[j]][i, ] <- list.values$chf[j, ] 
      }
    }
    vec.improve[h] <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                                       n.obs, n.death, n.groups)
  }
  vec.improve[-(n.set + 1)] <- 
    (-1) * (vec.improve[-(n.set + 1)] - vec.improve[n.set + 1])
  vec.improve 
}