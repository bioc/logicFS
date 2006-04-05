logic.bagging<-function(data,cl,B=100,ntrees=1,nleaves=8,glm.if.1tree=FALSE,
		anneal.control=logreg.anneal.control(),oob=TRUE,prob.case=0.5,
		importance=TRUE,rand=NULL){
	require(LogicReg) || stop("The package LogicReg is required.")
	if(!is.matrix(data))
		stop("'data' must be a matrix.")
	if(is.null(colnames(data))){
		colnames(data)<-paste("SNP",1:ncol(data),sep="")
		warning("Since 'data' has no column names, generic ones are added.",
			call.=FALSE)
	} 
	if(!is.null(rand))
		set.seed(rand)
	n<-length(cl)
	if(length(cl)!=nrow(data))
		stop("length(cl)!=nrow(data)")
	if(length(table(cl))!=2)
		stop("Currently only two-class analyses possible")
	type<-ifelse(ntrees>1 | glm.if.1tree,3,1)
	list.trees<-list.bagg<-vector("list",B)
	for(i in 1:B){
		bagg<-sample(n,n,replace=TRUE)
		list.trees[[i]]<-logreg(resp=cl[bagg],bin=data[bagg,],type=type,select=1,
			ntrees=ntrees,nleaves=nleaves,anneal.control=anneal.control)$model
		list.bagg[[i]]<-bagg
	}
	log.out<-list(logreg.model=list.trees,inbagg=list.bagg,data=data,type=type,
		ntrees=ntrees,nleaves=nleaves,cl=cl,oob.error=NULL,vim=NULL)
	class(log.out)<-"logicBagg"
	if(oob)
		log.out$oob.error<-logic.oob(log.out,prob.case=prob.case)
	if(importance)
		log.out$vim<-logic.vim(log.out,prob.case=prob.case)
	log.out
}

