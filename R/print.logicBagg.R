print.logicBagg <- function (x, ...) 
  {
    type <- x$type
    if (type == 9) 
      type <- 5
    cat("Bagged Logic Regression\n\n")
    cat("Number of Iterations: ", length(x$logreg.model), "\n")
    cat("Sampling Method:      ", x$sampling, "\n")
    cat("Logic Regression Type:", switch(type, "Classification", 
        "Linear Regression", "Logistic Regression", "Cox Regression", "Multinomial"), 
        "\n")
    cat("Search Algorithm:     ", ifelse(x$fast, "Stepwise Greedy Selection", 
                                         "Simulated Annealing"), "\n")
    if (x$type %in% c(2, 3, 4, 9)) 
      cat("Number of Trees:      ", if (x$nleaves >= 10) "", x$ntrees, "\n")
    cat("Max. Number of Leaves:", x$nleaves, "\n")
    if (!is.null(x$oob.error)){
      if (x$type == 2) 
        cat("\n", "OOB RMSPE:             ", round(x$oob.error, 3), "\n", sep = "")
      else 
        cat("\n", "OOB Error Rate:        ", 100 * round(x$oob.error, 4), "%", "\n", sep = "")
    }
  }
