getLmScore <- function (cl, mat.model, inbagg, oob)
{
  if (any(colSums(mat.model) == 0)) 
    mat.model <- mat.model[, colSums(mat.model) > 0, drop = FALSE]
  if (any(duplicated(mat.model, MARGIN = 2)))
    mat.model <- mat.model[, -which(duplicated(mat.model, MARGIN = 2)), 
                           drop = FALSE]
  if (ncol(mat.model) == 0){
    beta0 <- mean(cl[inbagg])
    rss.null <- mean((beta0 - cl[oob])^2)
    out <- log2(rss.null)
  } else {
    mat.model <- data.frame(cl = cl, mat.model)
    lm.out <- lm(cl ~ ., data = mat.model[inbagg, ])
    preds <- predict(lm.out, mat.model[oob, ])
    out <- log2(mean((preds - cl[oob])^2))
  }
  out
}