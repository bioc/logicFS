getModelEnsemble <-
function (mat.design, mat.new, oldcl, uni.death.times, n.death, score, rand)
  {
    n.trees <- ncol(mat.design)
    n.groups <- 2^(n.trees)
    VecGroup <- getVecGroup(n.trees)
    id.group.old <- mat.design[, , drop = FALSE] %*% VecGroup + 1
    mat.chf.groups <- matrix(0, nrow = n.groups, ncol = n.death) 
    index <- sort(unique(id.group.old))
    for (j in index){
      vec.chr <- if (score %in% c("Conc", "DPO")){numeric(n.death)} else {numeric(n.death) + 1}
      cl.temp <- oldcl[id.group.old == j] 
      chr.fit <- summary(survfit(cl.temp ~ 1))
      chr.temp <- if (score %in% c("Conc", "DPO")){cumsum(chr.fit$n.event / chr.fit$n.risk)} else {chr.fit$surv}
      time.temp <- chr.fit$time
      vec.chr[which(round(uni.death.times, 8) %in% round(time.temp, 8))] <- chr.temp 
      id.oob <- which(!(round(uni.death.times, 8) %in% round(time.temp, 8)))
      id.oob <- id.oob[id.oob != 1] 
      for (l in id.oob){
        vec.chr[l] <- vec.chr[l - 1]
      }
      mat.chf.groups[j, ] <- vec.chr
    }
    id.group.new <- mat.new %*% VecGroup + 1
    if (!all(unique(id.group.new) %in% unique(id.group.old))){
      id.special.oob <- which(!(id.group.new %in% unique(id.group.old)))
      if (!is.null(rand))
        set.seed(rand)
      id.group.new[id.special.oob] <- sample(unique(id.group.old), 
                                             length(id.special.oob), replace = TRUE)
    }
    out <- list(status = id.group.new, chf = mat.chf.groups) 
  }
