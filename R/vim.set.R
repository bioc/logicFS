`vim.set` <-
function(object,set=NULL,iter=NULL,standardize=FALSE,mu=0,addMatImp=FALSE,prob.case=0.5,rand=NA){
	if(!is(object,"logicBagg"))
		stop("object must be an object of class logicBagg.")
	if(!object$type%in%c(1,3))
		stop("Only available for classification and logistic regression.")
	cn<-colnames(object$data)
	n.var<-ncol(object$data)
	set<-checkSet(set,n.var,cn)
	if(object$type==1)
		mat.improve<-compMatImpSet1(object,set,iter=iter,rand=rand)
	else
		mat.improve<-compMatImpSet3(object,set,iter=iter,prob.case=prob.case,rand=rand)
	if(standardize)
		vim<-standardizeMatImp(mat.improve,mu=mu)
	else
		vim<-rowMeans(mat.improve)
	names(vim)<-rownames(mat.improve)<-names(set)
	if(!is.null(object$vim))
		param<-object$vim$param
	else
		param<-NULL
	measure<-paste(if(standardize) "Standardized\n", ifelse(is.null(iter),"Removing","Permutation"),
		"Based Set")
	if(standardize)
		threshold<-qt(1-0.05/nrow(mat.improve),ncol(mat.improve)-1)
	else
		threshold<-mu<-NULL
	if(!addMatImp)
		mat.improve<-NULL
	vim.out<-list(vim=vim,prop=NULL,primes=names(set),type=object$type,param=param,mat.imp=mat.improve,
		measure=measure,threshold=threshold,mu=mu,iter=iter)
	class(vim.out)<-"logicFS"
	vim.out
}

