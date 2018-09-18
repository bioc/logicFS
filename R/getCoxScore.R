getCoxScore <- function (cl, mat.model, inbagg, oob)
  {
    if (any(colSums(mat.model) == 0))
      mat.model <- mat.model[, colSums(mat.model) > 0, drop = FALSE]
    if (any(duplicated(mat.model, MARGIN = 2)))
      mat.model <- mat.model[, -which(duplicated(mat.model, MARGIN = 2)), 
                             drop = FALSE]
    if (ncol(mat.model) == 0){
      coef <- 1
      mat.model <- data.frame(cl = cl[oob], numeric(length(oob)))
      predPL(coef, mat.model)
    } else {
      mat.model <- data.frame(cl = cl, mat.model)
      surv.out <- coxph(cl ~ ., data = mat.model[inbagg,], ties = "breslow")
      coef <- surv.out$coefficients
      predPL(coef, mat.model[oob,])
    }
  }
