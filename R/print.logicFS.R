print.logicFS <- function (x, topX = 5, show.prop = TRUE, coded = FALSE, digits = 2, ...) 
{
  param <- x$param
  if (!is.null(param)){
    cat("Selection of Interactions Using Logic Regression\n\n")
    cat("Number of Iterations: ", param$B, "\n")
    cat("Sampling Method:      ", param$sampling, "\n")
    cat("Logic Regression Type:", switch(x$type, "Classification", 
                                         "Linear Regression", "Logistic Regression",
                                         "Cox Regression"), "\n")
    if (x$type %in% c(2, 3, 4)) 
      cat("Number of Trees:      ", param$ntrees, "\n")
    cat("Max. Number of Leaves:", param$nleaves, "\n\n")
  }
  cat("Importance Measure:", gsub("\n", "", x$measure), "\n")
  if (x$type %in% c(1, 3)) 
    cat("Based On:", ifelse(x$useN, "Number", "Proportion"), 
        "of", "OOB Observation", "\n")
  if (!is.null(x$mu)) 
    cat("(Alternative Hypothesis: Importance is greater than ", 
        x$mu, ")\n", sep = "")
  cat("\n")
  if (!is.null(param)) 
    cat("\n")
  vim <- sort(x$vim, decreasing = TRUE)
  topX <- min(topX, length(vim))
  names.vim <- if (coded) 
    names(vim)
  else x$primes[order(x$vim, decreasing = TRUE, na.last = NA)]
  if(!is.null(x$prop))
    prop <- as.numeric(x$prop)[order(x$vim, decreasing = TRUE, na.last = NA)]
  else
    prop <- NA
  out <- data.frame(Importance = vim, Proportion = prop, Expression = names.vim)
  if (!is.null(x$name)) 
    names(out)[3] <- x$name
  rownames(out) <- 1:nrow(out)
  if (!show.prop | is.null(x$prop)) 
    out <- out[, -2]
  out <- format(out[vim >= vim[topX], ], digits = digits, nsmall = 2)
  cat("The", nrow(out), "Most Important Interactions:\n\n")
  print(out)
}