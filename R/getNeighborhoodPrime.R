getNeighborhoodPrime <- function (x.prime, neighbor, set)
{
  name.snp <- numeric(length(x.prime))
  for (i in 1:length(x.prime)){
    name.snp[i] <- names(set)[colSums(sapply(set, function (x, b) x %in% b, 
                                             b = gsub("!", "", x.prime[i]))) > 0]
  }
  list.x.change <- vector("list", length(x.prime))
  if (any(name.snp %in% unlist(neighbor))){
    id.snp <- which(name.snp %in% unlist(neighbor))
    for (i in id.snp){
      id.neighbor <- which(sapply(neighbor, function (x, b) any(x %in% b), b = name.snp[i]))
      tmp.id1 <- which(sapply(set[neighbor[[id.neighbor]]], function (x, b) any(x %in% b), b = gsub("!", "", x.prime[i])))
      # Indize welcher SNP in neighbor quasi x.prime[i] entspricht
      tmp.id2 <- which(set[neighbor[[id.neighbor]]][[tmp.id1]] == gsub("!", "", x.prime[i]))
      # Indize ob dominant oder rezessiv
      list.x.change[[i]] <- sapply(set[neighbor[[id.neighbor]]][-tmp.id1], function (x, b) x[b], b = tmp.id2)
      if (gsub("!", "", x.prime[i]) != x.prime[i]) list.x.change[[i]] <- paste0("!", list.x.change[[i]])
      if (length(id.snp) > 1) list.x.change[[i]] <- c(x.prime[i], list.x.change[[i]])
    }
    data.change <- as.matrix(expand.grid(list.x.change[id.snp]))
    mat.new.prime <- matrix("", nrow = nrow(data.change), ncol = length(x.prime))
    mat.new.prime[, id.snp] <- data.change
    if (length(x.prime) != length(id.snp)){
      id.no.change <- which(!(1:length(x.prime) %in% id.snp))
      mat.new.prime[, id.no.change] <- rep(x.prime[id.no.change], each = nrow(mat.new.prime))
    }
    vec.out <- pasteColums(mat.new.prime)
    if (length(id.snp) > 1) vec.out <- vec.out[-1]
    # Urspruengliche Interaktion entfernt
    vec.out <- sortAlphabetically(vec.out)
  } else {vec.out <- NULL}
  
  return(vec.out)
}