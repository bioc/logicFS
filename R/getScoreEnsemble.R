getScoreEnsemble <- function (list.chf, mat.status, cl, score, n.obs, n.death, n.groups)
  {
    vec.n.oob <- 1 / colSums(mat.status != 0)
    mat.oob.eche <- matrix(0, nrow = n.obs, ncol = n.death)
    for (j in 1:n.groups){
      mat.tmp <- (mat.status == j) * 1
      mat.nxN <- t(mat.tmp) %*% list.chf[[j]]
      mat.oob.eche <- mat.oob.eche + mat.nxN
    }
    mat.oob.eche <- mat.oob.eche * vec.n.oob
    if (score == "Conc"){
      oob.eche <- rowSums(mat.oob.eche)
      imp <- getConc(oob.eche, cl)
    } else if (score == "DPO"){
      oob.eche <- rowSums(mat.oob.eche)
      imp <- getDPO(oob.eche, cl)
    } else {
      imp <- getBrierScoreEnsemble(mat.oob.eche, cl)
    }
    imp
  }
