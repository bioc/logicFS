sortAlphabetically <-
function(x)
{
  for( i in 1:length(x)){
    a <- unlist(strsplit(x[i], " & "))
    b <- gsub("!", "", a)
    d <- as.numeric(gsub("X", "", b))
    f <- a[order(d)]
    g <- f[1]
    if(length(f) > 1){
      for(j in 2:length(f)){
        g <- paste(g, f[j], sep = " & ")
      }
    }
    x[i] <- g
  }
  x
}
