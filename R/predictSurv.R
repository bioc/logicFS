predictSurv <- function (object, newData, score = c("DPO", "Conc", "Brier"))
{
  score <- match.arg(score)
  data <- object$data
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
  inbagg <- object$inbagg
  ltree <- object$logreg.model
  cl <- object$cl
  uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
  n.death <- length(uni.death.times)
  n.obs <- nrow(data)
  n.var <- ncol(data)
  n.groups <- 2^(object$ntrees)
  B <- length(ltree)
  mat.status <- matrix(0, nrow = B, ncol = n.obs)
  mat.status.newData <- matrix(0, nrow = B, ncol = nrow(newData))
  mat.sf <- matrix(0, nrow = B, ncol = n.death)
  list.sf <- vector("list", n.groups)
  for (j in 1:n.groups){
    list.sf[[j]] <- mat.sf
  }
  # Choose score depending on whether an SF or CHF estimate is required
  for (i in 1:B){
    tmp.ib <- inbagg[[i]]
    oob <- which(!(1:n.obs) %in% tmp.ib)
    mat.design <- sapply(ltree[[i]]$trees, eval.logreg, data[tmp.ib, ])
    mat.new <- sapply(ltree[[i]]$trees, eval.logreg, data[oob, ])
    # Estimate for SF or CHF
    list.values <- getModelEnsemble(mat.design, mat.new, cl[tmp.ib],  
                                    uni.death.times, n.death, score)
    mat.status[i, oob] <- list.values$status
    for (j in 1:n.groups){
      list.sf[[j]][i, ] <- list.values$chf[j, ] 
    }
    # ID group for newData
    mat.new <- sapply(ltree[[i]]$trees, eval.logreg, newData)
    VecGroup <- getVecGroup(ncol(mat.design))
    id.group.new <- as.numeric(mat.new %*% VecGroup + 1)
    id.group.old <- as.numeric(mat.design %*% VecGroup + 1)
    if (!all(unique(id.group.new) %in% unique(id.group.old))){
      id.special.oob <- which(!(id.group.new %in% unique(id.group.old)))
      id.group.new[id.special.oob] <- 0
      warning("Not in each iteration all OOB observations were considered for model evaluation.")
    }
    mat.status.newData[i, ] <- id.group.new
  }
  # Calculate model accuracy
  pred_score <- getScoreEnsemble(list.sf, mat.status, cl, score, 
                                 n.obs, n.death, n.groups)
  if (score != "Brier")
    pred_score <- 1 - pred_score
  names(pred_score) <- switch(score, DPO = "Prediction error based on DPO score",
                              Conc = "Prediction error based on Harrel's C-index",
                              Brier = "Brier score")
  # Predict SF or CHF based on newData
  sf <- matrix(0, nrow = nrow(newData), ncol = n.death)
  for (j in 1:n.groups){
    mat.tmp <- (mat.status.newData == j) * 1
    mat.nxN <- t(mat.tmp) %*% list.sf[[j]]
    sf <- sf + mat.nxN
  }
  sf <- sf / B
  if (is.null(rownames(newData)))
    rownames(sf) <- paste0("Obs", 1:nrow(newData))
  else 
    rownames(sf) <- rownames(newData)
  out <- list(sf = sf, pred_score = pred_score,
              uni.death.times = uni.death.times)
  # For predict.survivalFS exists an S3 print and plot version
  class(out) <- "predict.survivalFS"
  out
}