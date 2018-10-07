getSurvivalScore <- function (mat.model, inbagg, oob, cl, score, uni.death.times, n.death)
{
  mat.design <- mat.model[inbagg, , drop = FALSE]
  mat.new <- mat.model[oob, , drop = FALSE]
  oldcl <- cl[inbagg]
  newcl <- cl[oob]
  model.ensemble <- getModelEnsemble(mat.design, mat.new, oldcl, 
                                     uni.death.times, n.death, score)
  mat.oob.eche <- model.ensemble$chf[model.ensemble$status, ]
  if (score == "Conc"){
    oob.eche <- rowSums(mat.oob.eche)
    preds <- getConc(oob.eche, newcl)
  } else if (score == "DPO"){
    oob.eche <- rowSums(mat.oob.eche)
    preds <- getDPO(oob.eche, newcl)
  } else {
    preds <- getBrier(mat.oob.eche, newcl, uni.death.times)
  }  
  preds
}