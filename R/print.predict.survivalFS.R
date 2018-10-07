print.predict.survivalFS <- function (x, ...) 
{
  printscore <- names(x$pred_score)
  whatcurve <- ifelse(printscore == "Brier score", "Survival function", 
                      "Cumulative hazard function")
  
  cat(whatcurve, "prediction of", nrow(x$sf), "observations. \n")
  cat("\n")
  cat(printscore, "of prediction model:", x$pred_score, "\n")
  cat("Number of unique event times:", length(x$uni.death.times), "\n")
}