vim.ensemble <-
function (log.out, score = c("DPO", "Conc", "Brier"), adjusted = FALSE, 
                            neighbor = NULL, addMatImp = TRUE, addInfo = FALSE, rand = NULL)
  {
    if (!is(log.out, "logicBagg")) 
      stop("log.out must be an object of class logicBagg.")
    type <- log.out$type
    score <- match.arg(score)
    if(type != 4)
      stop("Ensemble measure only available for type = 4.")
    list.primes <- logic.pimp(log.out)
    allNull <- function (x) all(sapply(x, is.null))
    if (any(sapply(list.primes, allNull))){
      whichNull <- which(sapply(list.primes, allNull))
      log.out$logreg.model <- log.out$logreg.model[-whichNull]
      log.out$inbagg <- log.out$inbagg[-whichNull]
      list.primes <- logic.pimp(log.out)
      warning("Since ", length(whichNull), " of the models contain no variables, ", 
              "they are removed.", call. = FALSE)
    }
    B <- length(list.primes) 
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
        else lpi[!lpi %in% mono]
      }
      for (i in 1:B) list.primes[[i]] <- lapply(list.primes[[i]], 
                                                rmMonoPI, mono = mono, type = type)
    } 
    inbagg <- log.out$inbagg
    if (any(table(unlist(inbagg)) ==  B))
      stop("There are observations that are inbagg in all ", B,
           " iterations of survivalFS.\n", "Either choose a larger",
           " number of iterations or select a non-ensemble importance measure.")
    if (!is.null(neighbor)){
      if (length(unique(unlist(neighbor))) != length(unlist(neighbor)))
        stop("There are SNPs that have more than one neighborhood.")
      set <- checkSet(NULL, ncol(log.out$data), colnames(log.out$data))
      set <- lapply(set, function (x) paste0("X", x))
    } else {
      set <- NULL
    }
    if (adjusted){
      # vim <- getEnsembleAdjusted(log.out, vec.primes, 
      #                            list.primes, mat.eval, 
      #                            score, neighbor, set)
      stop("Still to come.")
    } else if (!is.null(neighbor)){
      #vim <- getEnsembleNeighbor(log.out, vec.primes, 
      #                            list.primes, mat.eval, 
      #                            score, neighbor, set)
      stop("Still to come.") 
    } else{
      vim <- getEnsemble(log.out, vec.primes, list.primes, 
                          mat.eval, score, rand) 
    }
    if (addMatImp) 
      sfm <- vim[length(vim)]
    else
      sfm <- NULL
    vim <- vim[-length(vim)]
    names(vim) <- vec.primes
    if (score == "Brier") 
      vim <- (-1) * vim
    measure <- switch(which(c("DPO", "Conc", "Brier") %in% score), "EDPO", 
                      "EConc", "EBrier")
    param <- if (addInfo) 
      list(B = B, ntrees = log.out$ntrees, nleaves = log.out$nleaves, 
           sampling = log.out$sampling)
    else NULL
    if (any(c(!is.null(neighbor), adjusted))){
      idm <- as.numeric(!is.null(neighbor)) + 2 * as.numeric(adjusted)
      measure <- paste(measure, switch(idm, "\n (LD adjusted)", "\n (noise adjusted)", 
                                       "\n (LD and noise adjusted)"), sep = "")
      prop <- getProp(list.primes, vec.primes, mat.eval, set, neighbor, adjusted)
    } else{
      prop <- prop[vec.primes]
    }
    primes <- getNames(vec.primes, colnames(log.out$data))
    vim.out <- list(vim = vim, prop = prop, primes = primes, 
                    type = type, score.full = sfm, B = B, 
                    measure = measure, name = "Interaction", 
                    neighbor = neighbor, param = param)
    class(vim.out) <- "logicFS"
    return(vim.out)
  }
