`vim.individual` <-
function(object,iter=NULL,prop=TRUE,standardize=FALSE,mu=0,addMatImp=FALSE,
		prob.case=0.5,rand=NA){
	if(!is(object,"logicBagg"))
		stop("object must be an object of class logicBagg.")
	if(!object$type%in%c(1,3))
		stop("Only available for classification and logistic regression.")
	if(object$type==1)
		mat.improve<-compMatImpIndividual1(object,iter=iter,rand=rand)
	else
		mat.improve<-compMatImpIndividual3(object,iter=iter,prob.case=prob.case,
			rand=rand)
	data<-object$data
	varnames<-colnames(data)
	if(standardize)
		vim<-standardizeMatImp(mat.improve,mu=mu)
	else
		vim<-rowMeans(mat.improve)
	names(vim)<-varnames
	if(prop){
		mat.prop<-getVarInTree(object$logreg.model,ncol(data))
		prop<-rowMeans(mat.prop)
		names(prop)<-varnames
	}
	else
		prop<-NULL
	if(!is.null(object$vim))
		param<-object$vim$param
	else
		param<-NULL
	measure<-paste(if(standardize) "Standardized\n", ifelse(is.null(iter),"Removing",
		"Permutation"),"Based Invididual")
	if(standardize)
		threshold<-qt(1-0.05/nrow(mat.improve),ncol(mat.improve)-1)
	else
		threshold<-mu<-NULL
	if(!addMatImp)
		mat.improve<-NULL
	vim.out<-list(vim=vim,prop=prop,primes=varnames,type=object$type,param=param,
		mat.imp=mat.improve,measure=measure,threshold=threshold,mu=mu,iter=iter)
	class(vim.out)<-"logicFS"
	vim.out
}

