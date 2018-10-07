survivalFS <- function(x, ...) UseMethod("survivalFS")


survivalFS.default <- function(x, y, B=20, replace=FALSE, sub.frac=0.632, 
  score = c("DPO", "Conc", "Brier", "PL"), addMatImp=TRUE, adjusted=FALSE, 
  neighbor=NULL, ensemble=FALSE, rand=NULL, ...){
  if(!is(y, "Surv"))
    stop("y must be an object of class Surv.")
  logicFS(x, y, B=B, replace=replace, sub.frac=sub.frac, score=score, addMatImp=addMatImp,
          ensemble=ensemble, adjusted=adjusted, neighbor=neighbor, rand=rand, ...)
}


survivalFS.formula <- function(formula, data, recdom=TRUE, ...){
	xy <- getXy(formula, data, recdom=TRUE)
	survivalFS(xy$x, xy$y, ...)
}


survivalFS.logicBagg <- function(x, score = c("DPO", "Conc", "Brier", "PL"), adjusted=FALSE, 
	neighbor=NULL, ensemble = FALSE, addMatImp = TRUE, rand=NULL, ...){
	vim.logicFS(x, neighbor=neighbor, adjusted=adjusted, score=score, ensemble=ensemble,
		addInfo=TRUE, addMatImp=addMatImp)
}


