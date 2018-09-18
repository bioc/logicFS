getVecImproveSet <-
function(object, set , score, rand)
{
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
  for(j in 1:n.groups) list.chf[[j]] <- mat.chf
  for (i in 1:B){
    tmp.ib <- inbagg[[i]]
    oob <- which(!(1:n.obs) %in% tmp.ib)
    if(is.null(set)){
      mat.design <- sapply(ltree[[i]]$trees, eval.logreg, data[tmp.ib, ])
      mat.new <- sapply(ltree[[i]]$trees, eval.logreg, data[oob, ])
    } else{
      data <- cbind(data, 1, 0)
      newtree <- lapply(ltree[[i]]$trees, getNewTree, set, n.var)
      newtree <- lapply(newtree, checkNewTree, n.var)
      if(any(sapply(newtree, is.null))){
        gmd <- function(x, y) 
        {
          if(is.null(x)) 
            return(matrix(0, nrow = nrow(y), ncol = 1))
          else
            return(eval.logreg(x, y))
        }
        mat.design <- sapply(newtree, gmd, data[tmp.ib, ])
        mat.new <- sapply(newtree, gmd, data[oob, ])
      } else{
        mat.design <- sapply(newtree, eval.logreg, data[tmp.ib, ])
        mat.new <- sapply(newtree, eval.logreg, data[oob, ])
      }
    } 
    list.values <- getModelEnsemble(mat.design, mat.new, cl[tmp.ib],  
                                      uni.death.times, n.death, score, rand)
    mat.status[i, oob] <- list.values$status
    for(j in 1:n.groups){
      list.chf[[j]][i, ] <- list.values$chf[j, ] 
    }
  }
  out <- getScoreEnsemble(list.chf, mat.status, cl, score, 
                     n.obs, n.death, n.groups)
  out
}
