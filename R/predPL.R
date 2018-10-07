predPL <- function (beta, mat.model)
{
  y <- mat.model$cl
  x <- mat.model[,-1, drop = FALSE]
  n.trees <- ncol(mat.model) - 1
  cens <- y[ , colnames(y) == "status"]
  y <- y[ , colnames(y) == "time"]
  sort.y <- y[order(y)]
  sort.cens <- cens[order(y)]
  sort.x <- x[order(y), , drop = FALSE]
  t <- sort.y[sort.cens == 1] 
  xt <- sort.x[sort.cens==1, , drop = FALSE] 
  vecTd <- t[!duplicated(t)]
  vec.d <- numeric(length(vecTd)) 
  mat.s <- matrix(numeric(length(vecTd)*n.trees), nrow = length(vecTd)) 
  for (i in 1:length(vecTd)){
    tmp.id <- which(t==vecTd[i])
    vec.d[i] <- length(tmp.id)
    mat.s[i,] <- colSums(xt[tmp.id, , drop = FALSE])
  }
  vec.ll1 <- mat.s%*%beta 
  n.death <- length(vecTd) 
  beta.x <- as.matrix(sort.x) %*% beta
  vec.ll2 <- numeric(n.death)
  for (i in 1:n.death){
    vec.ll2[i] <- sum(exp(beta.x[sort.y >= vecTd[i]]))^(vec.d[i])
  }
  sum(vec.ll1) - sum(log(vec.ll2)) 
}