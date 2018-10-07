predict.logicBagg <- function (object, newData, prob.case = 0.5, type = c("class", "prob"), 
                               score = c("DPO", "Conc", "Brier"), ...) 
{
  if (prob.case <= 0 | prob.case >= 1) 
    stop("prob.case must be between 0 and 1.")
  if (missing(newData)) 
    newData <- object$data
  else{
    if (any(is.na(newData))) 
      stop("No missing values allowed.")
    colOld <- colnames(object$data)
    if (!is.null(object$facInfo)) 
      newData <- getXyPred(newData, object$facInfo, colOld)
    else{
      colNew <- colnames(newData)
      if (length(colOld) != length(colNew) || any(colOld != 
                                                  colNew)) 
        stop("newData must contain the same variables in the same order as\n", 
             "the data matrix in logic.bagging (without the response if the\n", 
             "formula method has been used).")
    }
    newData <- as.matrix(newData)
    if (any(!newData %in% c(0, 1))) 
      stop("newData must only contain binary variables coded by 0 and 1.")
  }
  trees <- object$logreg.model
  type <- match.arg(type)
  if (object$type == 9) {
    levs <- levels(object$cl)
    return(predictMLB(trees, newData, levs, type = type))
  }
  if (object$type == 4){
    score <- match.arg(score)
    return(predictSurv(object, newData, score))
  }
  n.new <- nrow(newData)
  B <- length(trees)
  mat.pred <- matrix(NA, n.new, B)
  for (i in 1:B) mat.pred[, i] <- predict(trees[[i]], newData, 
                                          object$type)
  pred <- rowMeans(mat.pred)
  if (object$type == 2 | type == "prob") 
    return(pred)
  pred <- as.numeric(pred > prob.case)
  if (any(pred == prob.case)) 
    pred[pred == prob.case] <- sample(0:1, sum(pred == prob.case), 
                                      replace = TRUE)
  pred
}