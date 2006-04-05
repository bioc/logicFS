logic.fs<-function(data,cl,B=100,ntrees=1,nleaves=8,glm.if.1tree=FALSE,
		anneal.control=logreg.anneal.control(),prob.case=0.5,rand=NULL){
	log.out<-logic.bagging(data,cl,B=B,ntrees=ntrees,nleaves=nleaves,importance=FALSE,
		glm.if.1tree=glm.if.1tree,anneal.control=anneal.control,oob=FALSE,
		prob.case=prob.case,rand=rand)
	vim.out<-logic.vim(log.out,prob.case=prob.case,addInfo=TRUE)
	vim.out
}

