check.neighborint <- function (neighborint)
{
  k <- 1
  while (any(names(neighborint) %in% unlist(neighborint))){
    neighborint[neighborint[[k]]] <- NULL
    k <- k + 1
  }
  neighborint
}