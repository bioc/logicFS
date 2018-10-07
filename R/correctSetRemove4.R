correctSetRemove4 <- function (ltree, data, cl, tmp.ib, oob, set, 
                               n.var, n.set, score) 
{
  # 1. Calculate score of full model
  if (score != "PL"){
    uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
    n.death <- length(uni.death.times)
  }
  mat.model <- sapply(ltree$trees, eval.logreg, data)
  if (score == "PL"){
    cortree <- getCoxScore(cl, mat.model, tmp.ib, oob)
  } else {
    cortree <- getSurvivalScore(mat.model, tmp.ib, oob, cl, score, 
                                uni.death.times, n.death)
  }
  data <- cbind(data, 1, 0)
  corpreds <- numeric(n.set)
  id.set <- which(unlist(lapply(set, function (x)
  {any(x %in% unique(ltree$trees[[1]]$trees$knot))})))
  for (i in id.set) {
    newtree <- lapply(ltree$trees, getNewTree, set[[i]], 
                      n.var)
    # Hier werden Variablen fuer die einzelnen SNPs durch Vektor mit 1en bzw. 0en ersetzt 
    newtree <- lapply(newtree, checkNewTree, n.var)
    ids <- !unlist(lapply(newtree, is.null))
    if (sum(ids) == 0) {
      mat.model <- matrix(1, nrow = nrow(data))
    } else {
      newtree <- newtree[ids]
      mat.model <- sapply(newtree, eval.logreg, data)
    }
    if (score == "PL"){
      preds <- getCoxScore(cl, mat.model, tmp.ib, oob)
    } else {
      preds <- getSurvivalScore(mat.model, tmp.ib, oob, cl, score, 
                                uni.death.times, n.death)
    }
    corpreds[i] <- preds - cortree
  }
  if (score == "PL"){
    corpreds <- -2 * corpreds
  } else if (score != "Brier"){
    corpreds <- -1 * corpreds
  }
  corpreds
}