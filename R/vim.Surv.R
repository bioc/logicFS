vim.Surv <-
function (mprimes, mat.eval, inbagg, cl, score, rand)
  {
    if (score != "PL"){
      uni.death.times <- sort(unique(cl[, 1][cl[, 2] == 1]))
      n.death <- length(uni.death.times)
    }
    primes <- unique(unlist(mprimes))
    n.trees <- length(mprimes)
    n.primes <- length(primes)
    id.primes <- colnames(mat.eval) %in% primes
    oob <- which(!(1:nrow(mat.eval)) %in% inbagg)
    vec.improve <- numeric(ncol(mat.eval))
    names(vec.improve) <- colnames(mat.eval)
    if ((n.primes == 1) & (score == "PL")){
      vec.improve[id.primes] <- vimCox.oneprime(mat.eval[, id.primes], 
                                                cl, oob, inbagg)
      return(vec.improve)
    }
    mat.in <- cbind(1 - diag(n.primes), 1)
    rownames(mat.in) <- primes
    list.eval <- lapply(mprimes, function (x, e = mat.eval, i = mat.in) 
      e[, x, drop = FALSE] %*% i[x, , drop = FALSE] > 0)
    le.vec <- n.primes + 1
    vec.ll <- numeric(le.vec)
    getIth <- function (x, ids = NULL) x[, ids]
    for (i in 1:le.vec){
      mat.model <- matrix(unlist(lapply(list.eval, getIth, 
                                        ids = i)), ncol = n.trees)
      if (score == "PL"){
        vec.ll[i] <- getCoxScore(cl, mat.model, inbagg, oob)
      } else {vec.ll[i] <- getSurvivalScore(mat.model, inbagg, oob, cl, score, 
                                             uni.death.times, n.death, rand)
      }
    }
    vec.ll <- vec.ll - vec.ll[le.vec]
    vec.improve[primes] <- vec.ll[-le.vec]
    if (score == "PL"){
      vec.improve <- -2 * vec.improve
    } else if (score != "Brier"){
      vec.improve <- -1 * vec.improve
    }
    vec.improve
  }
