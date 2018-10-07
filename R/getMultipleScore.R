getMultipleScore <- function (cl, mat.model, inbagg, oob, prob.case)
{
  if (any(colSums(mat.model) == 0)) 
    mat.model <- mat.model[, colSums(mat.model) > 0, drop = FALSE]
  if (any(duplicated(mat.model, MARGIN = 2)))
    mat.model <- mat.model[, -which(duplicated(mat.model, MARGIN = 2)), 
                           drop = FALSE]
  if (ncol(mat.model) == 0){
    cl.train <- cl[-oob]
    coef.null <- log(sum(cl.train == 1)/sum(cl.train == 0))
    out <- ifelse(exp(coef.null) / (1- exp(coef.null)) > prob.case, 
                  sum(cl[oob]), sum(1 - cl[oob])) 
  } else {
    mat.model <- data.frame(cl = cl, mat.model)
    glm.out <- glm(cl ~ ., data = mat.model[inbagg, ], family = "binomial")
    preds <- predict(glm.out, mat.model[oob, ], type = "response") > 
      prob.case
    out <- sum(preds == cl[oob])
  }
  out
}