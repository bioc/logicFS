`logicFS` <-
function(x,...) UseMethod("logicFS")


`logicFS.formula` <-
function(formula,data,...){
	if(!is.data.frame(data))
		stop("data must be a data frame.")
	mf<-model.frame(formula,data=data)
	y<-model.response(mf)
	x<-model.matrix(formula,mf)[,-1,drop=FALSE]
	logicFS(x,y,...)
}


`logicFS.default` <-
function(x,y,B=100,ntrees=1,nleaves=8,glm.if.1tree=FALSE,
		replace=TRUE,sub.frac=0.632,anneal.control=logreg.anneal.control(),
		prob.case=0.5,addMatImp=TRUE,rand=NULL,...){
	log.out<-logic.bagging(x,y,B=B,ntrees=ntrees,nleaves=nleaves,importance=FALSE,
		glm.if.1tree=glm.if.1tree,replace=replace,sub.frac=sub.frac,
		anneal.control=anneal.control,oob=FALSE,prob.case=prob.case,rand=rand)
	vim.out<-vim.logicFS(log.out,prob.case=prob.case,addInfo=TRUE,addMatImp=addMatImp)
	vim.out
}

