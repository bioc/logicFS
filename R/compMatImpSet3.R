compMatImpSet3 <-
function (object, set, useN = FALSE, iter = NULL, prob.case = 0.5, 
                            score = c("DPO", "Conc", "Brier", "PL"), rand = NULL) 
{
  inbagg <- object$inbagg
  ltree <- object$logreg.model
  data <- object$data
  cl <- object$cl
  n.var <- ncol(data)
  n.obs <- nrow(data)
  n.set <- length(set)
  B <- length(ltree)
  mat.improve <- matrix(0, n.set, B)
  if((object$type == 4) && (score != "PL"))
    uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
  tmp.fun <- paste("correctSet", ifelse(is.null(iter), "Remove", 
                                        "Permute"), object$type, sep = "")
  FUN <- match.fun(tmp.fun)
  if (!is.null(iter) && !is.null(rand)) 
    set.seed(rand)
  for (i in 1:B) {
    tmp.ib <- inbagg[[i]]
    oob <- which(!(1:n.obs) %in% tmp.ib)
    if (is.null(iter)){
      if((object$type != 4) | ((object$type == 4) & (score == "PL")))
        mat.improve[, i] <- FUN(ltree[[i]], data[tmp.ib, ], data[oob, ], 
                                cl[tmp.ib], cl[oob], set, n.var, n.set, 
                                prob.case = prob.case)
      else
        mat.improve[, i] <- correctSetRemove4Ensemble(ltree[[i]], data, cl, tmp.ib, oob,
                                           n.var, uni.death.times, set, score, rand)
    }else mat.improve[, i] <- FUN(ltree[[i]], data[oob, ], 
                                  cl[oob], set, n.var, n.set, iter, prob.case = prob.case)
    if (object$type == 3 && !useN) 
      mat.improve[, i] <- mat.improve[, i]/length(oob)
  }
  mat.improve
}
