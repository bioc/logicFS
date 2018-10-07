vim.set.ensemble <- function (object, score = c("DPO", "Conc", "Brier"), 
                              addMatImp = FALSE, set = NULL)
{
  score <- match.arg(score)
  if (!is(object, "logicBagg")) 
    stop("object must be an object of class logicBagg.")
  if (object$type != 4) 
    stop("Ensemble measures are only available in the survival case.")
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
  B <- length(object$logreg.model) 
  if (any(table(unlist(object$inbagg)) ==  B))
    stop("There are observations that are inbagg in all ", B,
         " iterations of survivalFS.\n", "Either choose a greater",
         " number of iterations or select a non-ensemble importance measure.")
  cn <- colnames(object$data)
  n.var <- ncol(object$data)
  set <- checkSet(set, n.var, cn)
  vim <- getSetEnsemble(object, set, score) 
  if (addMatImp) 
    sfm <- vim[length(vim)]
  else
    sfm <- NULL
  vim <- vim[-length(vim)]
  names(vim) <- names(set)
  if (score == "Brier") 
    vim <- (-1) * vim
  measure <- paste0("Removing Based SNP using ", 
                    switch(which(c("DPO", "Conc", "Brier") %in% score), 
                           "EDPO", "EConc", "EBrier"), " score")
  vim.out <- list(vim = vim, prop = NULL, primes = names(set), 
                  type = object$type, param = NULL, mat.imp = sfm, 
                  measure = measure, useN = TRUE, threshold = NULL, 
                  mu = NULL, iter = NULL, name = "Set")
  class(vim.out) <- "logicFS"
  vim.out
}