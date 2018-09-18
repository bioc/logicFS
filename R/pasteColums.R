pasteColums <-
function(x, sep = " & ")
{
  b <- x[, 1]
  if(ncol(x) > 1){
    for(i in 2:ncol(x)){
      a <- x[, i]
      b <- paste(b, a, sep = sep)
    }
  }
  b
}
