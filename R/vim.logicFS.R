vim.logicFS <- function (log.out, neighbor = NULL, adjusted = FALSE, useN = TRUE, 
                           onlyRemove = FALSE, prob.case = 0.5, addInfo = FALSE, 
                           score = c("DPO", "Conc", "Brier", "PL"), ensemble = TRUE,
                           addMatImp = TRUE, rand = NULL) 
  {
    if (!is(log.out, "logicBagg")) 
      stop("log.out must be an object of class logicBagg.")
    type <- log.out$type
    if (!type %in% c(1:4, 9)) 
      stop("Currently only available for classification and\n", 
           "linear, (multinomial) logistic and Cox regression.")
    if(!(type %in% c(1:4)) & (adjusted | !is.null(neighbor)))
      stop("Noise adjusted and/or LD adjusted measure only available for type 1-4.")
    score <- match.arg(score)
    if (type == 4 & ensemble){
      out <- vim.ensemble(log.out, score = score, adjusted = adjusted, 
                          neighbor = neighbor, addMatImp = addMatImp, rand = rand)
      return(out)
    }
    if (type != 1) 
      onlyRemove <- TRUE 
    if(!onlyRemove & (adjusted | !is.null(neighbor)))
      stop("Noise adjusted and/or LD adjusted measure not available for onlyRemove = FALSE")
    list.primes <- logic.pimp(log.out)
    if (type == 4){
      allNull <- function (x) all(sapply(x, is.null))
      if (any(sapply(list.primes, allNull))){
        whichNull <- which(sapply(list.primes, allNull))
        log.out$logreg.model <- log.out$logreg.model[-whichNull]
        log.out$inbagg <- log.out$inbagg[-whichNull]
        list.primes <- logic.pimp(log.out)
        warning("Since ", length(whichNull), " of the models contain no variables, ", 
                "they are removed.", call. = FALSE)
      }
    }
    if ((type != 1 | onlyRemove) & (type != 4)) 
      list.primes <- check4NullModels(list.primes)
    B <- length(list.primes)
    if (type == 1)
      vec.primes <- unlist(list.primes) 
    else 
      vec.primes <- unlist(lapply(list.primes, function (x) unique(unlist(x))))
    prop <- table(vec.primes)/B 
    vec.primes <- unique(vec.primes) 
    data <- as.data.frame(log.out$data)
    colnames(data) <- paste("X", 1:ncol(data), sep = "")
    mat.eval <- getMatEval(data, vec.primes, check = FALSE) 
    if (ncol(mat.eval) < length(vec.primes)){
      ids <- which(!vec.primes %in% colnames(mat.eval))
      mono <- vec.primes[ids]
      vec.primes <- vec.primes[-ids]
      rmMonoPI <- function (lpi, mono, type){
        if (type == 9) 
          lapply(lpi, function (x) x[!x %in% mono])
        else 
          lpi[!lpi %in% mono]
      }
      for (i in 1:B) 
         list.primes[[i]] <- lapply(list.primes[[i]], rmMonoPI, mono = mono, type = type)
    } 
    cl <- log.out$cl
    inbagg <- log.out$inbagg
    if (class(cl) != "Surv"){
      n.cl <- length(cl)
    } else {
      n.cl <- length(cl[, 1])
    }
    mat.imp <- matrix(0, length(vec.primes), B)  
    rownames(mat.imp) <- colnames(mat.eval)
    if (!is.null(neighbor)){
      if (length(unique(unlist(neighbor))) != length(unlist(neighbor)))
        stop("There are SNPs that have more than one neighborhood.")
      set <- checkSet(NULL, ncol(log.out$data), colnames(log.out$data))
      set <- lapply(set, function (x) paste0("X", x))
    } else {
      set <- NULL
    }
    if (type == 1){
      vim.fun <- ifelse(onlyRemove, "vim.singleRemove", "vim.singleBoth") 
      FUN <- match.fun(vim.fun)
      for (i in 1:B){
        oob <- which(!(1:n.cl) %in% inbagg[[i]])
        if (adjusted)
          mat.imp[, i] <- vim.singleAdjusted(list.primes[[i]][[1]], mat.eval[oob, ], 
                                             cl[oob], neighbor, set, useN = useN)
        else if (!is.null(neighbor))
          mat.imp[, i] <- vim.singleNeighbor(list.primes[[i]][[1]], mat.eval[oob, ], 
                                             cl[oob], neighbor, set, useN = useN)
        else 
          mat.imp[, i] <- FUN(list.primes[[i]][[1]], mat.eval[oob, ], cl[oob], useN = useN)
      }
    }
    if (type == 2){
      cat("Note: Since version 1.15.8 log2(MSEP) instead of MSEP is used to quantify", 
          "\n", "the importance of the interactions for predicting a ", 
          "quantitative response.", "\n\n", sep = "")
      list.primes <- check.listprimes(list.primes, log.out$ntrees, B)
      for (i in 1:B){
        if (adjusted)
          mat.imp[, i] <- vim.lmAdjusted(list.primes[[i]], mat.eval, inbagg[[i]], 
                                         cl, neighbor, set)
        else if (!is.null(neighbor))
          mat.imp[, i] <- vim.lmNeighbor(list.primes[[i]], mat.eval, inbagg[[i]], 
                                         cl, neighbor, set)
        else 
          mat.imp[, i] <- vim.lm(list.primes[[i]], mat.eval, inbagg[[i]], cl)
      }
    }
    if (type == 3){
      list.primes <- check.listprimes(list.primes, log.out$ntrees, B)
      for (i in 1:B){
        if (adjusted)
          mat.imp[, i] <- vim.multipleAdjusted(list.primes[[i]], mat.eval, inbagg[[i]], cl, 
                                               prob.case = prob.case, neighbor, set, useN = useN)
        else if (!is.null(neighbor))
          mat.imp[, i] <- vim.multipleNeighbor(list.primes[[i]], mat.eval, inbagg[[i]], cl, 
                                               prob.case = prob.case, neighbor, set, useN = useN)
        else 
          mat.imp[, i] <- vim.multiple(list.primes[[i]], mat.eval, inbagg[[i]], 
                                       cl, prob.case = prob.case, useN = useN)
      }
    }
    if (type == 4){
      list.primes <- check.listprimes(list.primes, log.out$ntrees, B)
      for (i in 1:B){
        if (adjusted)
          mat.imp[, i] <- vim.SurvAdjusted(list.primes[[i]], mat.eval, inbagg[[i]], 
                                           cl, neighbor, set, score)
        else if (!is.null(neighbor))
          mat.imp[, i] <- vim.SurvNeighbor(list.primes[[i]], mat.eval, inbagg[[i]], 
                                           cl, neighbor, set, score)
        else 
          mat.imp[, i] <- vim.Surv(list.primes[[i]], mat.eval, inbagg[[i]], 
                                   cl, score, rand)
      }
    }
    if (type == 9){
      n.reg <- length(levels(cl)) - 1
      list.primes <- lapply(list.primes, check.listprimes, ntrees = log.out$ntrees, B = n.reg)
      lmodel <- log.out$logreg.model
      for (i in 1:B) mat.imp[, i] <- vim.MLR(lmodel[[i]], list.primes[[i]], 
                                             mat.eval, log.out$data, cl, 
                                             inbagg[[i]], useN = useN)$vec.out
    }
    vim <- rowMeans(mat.imp, na.rm = TRUE)
    if (!is.null(neighbor) | adjusted){
      prop <- getProp(list.primes, vec.primes, mat.eval, 
                       set, neighbor, adjusted)
    } else{
      prop <- prop[vec.primes]
    }
    primes <- getNames(vec.primes, colnames(log.out$data))
    param <- if (addInfo) 
      list(B = B, ntrees = log.out$ntrees, nleaves = log.out$nleaves, 
           sampling = log.out$sampling)
    else NULL
    if (!addMatImp) 
      mat.imp <- NULL 
    if (type == 9) 
      measure <- "Multiple Tree"
    else measure <- switch(type, "Single Tree", "Quantitative Response", "Multiple Tree", 
                           switch(which(c("DPO", "Conc", "Brier", "PL") %in% score), 
                                  "DPO", "Conc", "Brier", "Cox")) 
    if (type == 1 && onlyRemove && is.null(neighbor) && !adjusted)
      measure <- paste(measure, "\n (Only Removing)", sep = "")
    if (any(c(!is.null(neighbor), adjusted))){
      idm <- as.numeric(!is.null(neighbor)) + 2 * as.numeric(adjusted)
      measure <- paste(measure, switch(idm, "\n (LD adjusted)", "\n (noise adjusted)", 
                                       "\n (LD and noise adjusted)"), sep = "")
    }
    vim.out <- list(vim = vim, prop = prop, primes = primes, type = type, 
                    param = param, mat.imp = mat.imp, measure = measure, 
                    useN = useN, neighbor = neighbor, threshold = NULL, 
                    mu = NULL, B = B)
    class(vim.out) <- "logicFS"
    vim.out
  }
