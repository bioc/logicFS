vim.set.ensemble <-
function (object, score = c("DPO", "Conc", "Brier"), 
                          addMatImp = FALSE, set = NULL, rand = NULL)
{
  score <- match.arg(score)
  if (object$type != 4) 
    stop("Unknown type.")
  B <- length(object$logreg.model) 
  if(any(table(unlist(object$inbagg)) ==  B))
    stop("There are observations that are inbagg in all B iterations.\n", 
         "Either choose a greater number of iterations or select PL Score")
  cn <- colnames(object$data)
  n.var <- ncol(object$data)
  set <- checkSet(set, n.var, cn)
  n.set <- length(set)
  vec.improve <- numeric(n.set + 1)
  
  vec.improve[n.set + 1] <- getVecImproveSet(object, NULL, score, rand) 
  for(h in 1:(n.set)){
    vec.improve[h] <- getVecImproveSet(object, set[[h]], score, rand)
  }
  names(vec.improve) <- c(names(set), "Full Model")
  c.index <- if(score == "Brier"){
    (vec.improve - vec.improve[n.set + 1]) 
  } else{
    -(vec.improve - vec.improve[n.set + 1])
  } 
  vim <- c.index[-(n.set + 1)]
  names(vim) <- names(set)
  if(score == "Conc"){
    measure <- "Ensemble Concordance Index" 
  } else if (score == "DPO"){
    measure <- "Ensemble DPO"
  } else{
    measure <- "Ensemble Brier Score"
  }  
  if(!addMatImp)
    vec.improve <- NULL
  vim.out <- list(vim = vim, prop = NULL, primes = names(set), 
                  type = object$type, vec.imp = vec.improve, 
                  measure = measure, B = B, name = "Set")
  class(vim.out) <- "logicFS"
  vim.out
}
