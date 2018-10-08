logicFS <- function(x,...) UseMethod("logicFS")


logicFS.formula <- function(formula,data,recdom=TRUE,...){
	xy<-getXy(formula,data,recdom=recdom)
	logicFS(xy$x,xy$y,...)
}


logicFS.default <- function(x,y,B=100,useN=TRUE,ntrees=1,nleaves=8,glm.if.1tree=FALSE,
		replace=TRUE,sub.frac=0.632,anneal.control=logreg.anneal.control(),
		onlyRemove=FALSE,prob.case=0.5,score = c("DPO", "Conc", "Brier", "PL"),
		addMatImp=TRUE,fast=FALSE,neighbor = NULL, adjusted = FALSE, ensemble = FALSE,
		rand=NULL,...){
	log.out<-logic.bagging(x,y,B=B,ntrees=ntrees,nleaves=nleaves,importance=FALSE,
		glm.if.1tree=glm.if.1tree,replace=replace,sub.frac=sub.frac,
		anneal.control=anneal.control,oob=FALSE,prob.case=prob.case,
		fast=fast,rand=rand)
	vim.out<-vim.logicFS(log.out, neighbor = neighbor, adjusted = adjusted, useN=useN,
		onlyRemove=onlyRemove,prob.case=prob.case,
		addInfo=TRUE, score=score, addMatImp=addMatImp)
	vim.out
}

logicFS.logicBagg<-function(x, neighbor = NULL, adjusted = FALSE, prob.case = 0.5,
		score = c("DPO", "Conc", "Brier", "PL"), ensemble = FALSE, 
		addMatImp = TRUE, ...)
	vim.logicFS(x, neighbor = neighbor, adjusted = adjusted, prob.case = prob.case,
		score = score, ensemble = ensemble, addInfo = TRUE, addMatImp = addMatImp)




