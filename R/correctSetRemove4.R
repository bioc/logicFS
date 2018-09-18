correctSetRemove4 <-
function (ltree, olddata, newdata, oldcl, newcl, set, n.var, n.set, prob.case = 0.5) 
{
  mat.design <- sapply(ltree$trees, eval.logreg, olddata)
  coef <- coxph(oldcl ~ mat.design, ties = "breslow")$coefficients
  mat.new <- sapply(ltree$trees, eval.logreg, newdata)
  mat.model <- data.frame(cl = newcl, mat.new)
  cortree <- predPL(coef, mat.model)
  
  newdata <- cbind(newdata, 1, 0)
  olddata <- cbind(olddata, 1, 0)
  corpreds <- numeric(n.set)
  for (i in 1:n.set) {
    newtree <- lapply(ltree$trees, getNewTree, set[[i]], 
                      n.var) 
    newtree <- lapply(newtree, checkNewTree, n.var)
    ids <- !unlist(lapply(newtree, is.null))
    if (sum(ids) == 0) {
      mat.design <- matrix(1, nrow = nrow(olddata))
      mat.new <- matrix(1, nrow = nrow(newdata))
    }else {
      newtree <- newtree[ids]
      mat.design <- sapply(newtree, eval.logreg, olddata)
      mat.new <- sapply(newtree, eval.logreg, newdata)
    }
    data.design <- data.frame(cl = oldcl, mat.design)
    coef <- coxph(cl ~ ., data = data.design, ties = "breslow")$coefficients
    data.new <- data.frame(cl = newcl, mat.new)
    preds <- predPL(coef, data.new) 
    corpreds[i] <- -2*(preds - cortree)
  }
  corpreds
}
