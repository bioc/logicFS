correctSetRemove4Ensemble <-
function(mtree, data, cl, inbagg, oob, n.var, 
                                      uni.death.times, set, score, rand)
{
  # Erst score (cortree) für volles Modell berechnen:
  n.death <- length(uni.death.times)
  mat.model <- sapply(mtree$trees, eval.logreg, data)
  cortree <- getSurvivalScore(mat.model, inbagg, oob, cl, score, uni.death.times, n.death)
  
  # Jetzt Scores für reduzierte Modelle berechnen 
  data <- cbind(data, 1, 0)
  n.set <- length(set)
  # Nur die berechen die wirklich notwendig sind:
  tmp.vec.snp <- numeric(n.set)
  for(i in 1:n.set){
    for(j in 1:length(mtree$trees))
      if(any(mtree$trees[[j]]$trees$knot %in% set[[i]]))
        tmp.vec.snp[i] <- 1
  }
  id.snp <- which(tmp.vec.snp == 1)
  corpreds <- numeric(n.set)
  for (i in id.snp) {
    newtree <- lapply(mtree$trees, getNewTree, set[[i]], 
                      n.var)
    newtree <- lapply(newtree, checkNewTree, n.var)
    if(any(sapply(newtree, is.null))){
      gmd <- function(x, y) 
      {
        if(is.null(x)) 
          return(matrix(0, nrow = nrow(y), ncol = 1))
        else
          return(eval.logreg(x, y))
      }
      mat.model <- sapply(newtree, gmd, data)
    } else{
      mat.model <- sapply(newtree, eval.logreg, data)
    }
    preds <- getSurvivalScore(mat.model, inbagg, oob, cl, score, 
                               uni.death.times, n.death)
    corpreds[i] <- -(preds - cortree)
  }
  if(score == "Brier")
    corpreds <- -1 * corpreds
  corpreds
}
