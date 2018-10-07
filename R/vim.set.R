vim.set <- function (object, set = NULL, useN = NULL, iter = NULL, standardize = NULL, 
                     mu = 0, addMatImp = FALSE, prob.case = 0.5, 
                     score = c("DPO", "Conc", "Brier", "PL"), ensemble = FALSE, rand = NULL) 
{
  if (!is(object, "logicBagg")) 
    stop("object must be an object of class logicBagg.")
  if (!object$type %in% c(1:4, 9)) 
    stop("Only available for classification, linear,\n", 
         "(multinomial) logistic and survival regression.")
  if (is.null(standardize)) 
    standardize <- !(object$type %in% c(2, 4))
  if (object$type == 2) {
    cat("Note: Since version 1.15.8 log2(MSEP) instead of MSEP is used to quantify", 
        "\n", "the importance of the (sets of) SNPs for predicting a ", 
        "quantitative response.", "\n\n", sep = "")
    if (standardize) 
      warning("In the linear regression case, no standardization should be done.")
  }
  if (is.null(useN)){
    if (object$type == 4)
      useN <- TRUE
    else{
      useN <- object$vim$useN
      if (is.null(useN)) 
        stop("useN needs to be specified when importance = FALSE in logic.bagging.")
    }
  }
  if (object$type == 4){
    if(standardize)
      stop("Standardization currently not available in the survival case.")
    if(!is.null(iter)){
      iter <- NULL
      warning("Permutation of variables not available in the survival case. \n",
              "Therefore iter is set to NULL.")
    }
    if(!useN){
      useN <- TRUE
      warning("In the survival case useN is ignored.")
    }
  }
  
  score <- match.arg(score)
  if (ensemble){
    out <- vim.set.ensemble(object, score = score, addMatImp = addMatImp)
    return(out)
  }
  
  if (object$type == 4){
    list.primes <- logic.pimp(object)
    allNull <- function (x) all(sapply(x, is.null))
    if (any(sapply(list.primes, allNull))){
      whichNull <- which(sapply(list.primes, allNull))
      object$logreg.model <- object$logreg.model[-whichNull]
      object$inbagg <- object$inbagg[-whichNull]
      list.primes <- logic.pimp(object)
      warning("Since ", length(whichNull), " of the models contain no variables, ",
              "they are removed.", call. = FALSE)
    }
  }
  cn <- colnames(object$data)
  n.var <- ncol(object$data)
  set <- checkSet(set, n.var, cn)
  
  if (object$type == 1) 
    mat.improve <- compMatImpSet1(object, set, useN = useN, 
                                  iter = iter, rand = rand)
  else mat.improve <- compMatImpSet3(object, set, useN = useN, 
                                     iter = iter, prob.case = prob.case, 
                                     score = score, rand = rand)
  
  if (standardize) 
    vim <- standardizeMatImp(mat.improve, mu = mu)
  else vim <- rowMeans(mat.improve, na.rm = TRUE) 
  names(vim) <- rownames(mat.improve) <- names(set)
  measure <- paste0(if(object$type != 4) paste(if (standardize) "Standardized \n", 
                                               ifelse(is.null(iter), "Removing", "Permutation"), "Based Set"), 
                    if((object$type == 4)) paste("Removing Based Set using", 
                                                 switch(which(c("DPO", "Conc", "Brier", "PL") %in% score), 
                                                        "DPO score", "Conc score", "Brier score", "Cox score")))
  if (standardize) 
    threshold <- qt(1 - 0.05/nrow(mat.improve), ncol(mat.improve) - 1)
  else {
    if (object$type == 2) 
      threshold <- qf(1 - 0.05/nrow(mat.improve), ncol(mat.improve), 
                      ncol(mat.improve))
    else threshold <- mu <- NULL
  }
  if (!addMatImp) 
    mat.improve <- NULL
  vim.out <- list(vim = vim, prop = NULL, primes = names(set), 
                  type = object$type, param = NULL, mat.imp = mat.improve, 
                  measure = measure, useN = useN, threshold = threshold, 
                  mu = mu, iter = iter, name = "Set")
  class(vim.out) <- "logicFS"
  vim.out
}