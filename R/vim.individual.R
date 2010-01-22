vim.individual <- function(...)
	cat("vim.individual has been renamed to vim.input. So please use vim.input instead.\n")


`vim.input` <-
function(object,useN=NULL,iter=NULL,prop=TRUE,standardize=NULL,mu=0,addMatImp=FALSE,
		prob.case=0.5,rand=NA){
	if(!is(object,"logicBagg"))
		stop("object must be an object of class logicBagg.")
	if(!object$type%in%c(1:3,9))
		stop("Only available for classification and linear and\n",
			"(multinomial) logistic regression.")
	if(is.null(standardize))
		standardize <- object$type != 2
	if(object$type==2){
		cat("Note: Since version 1.15.8 log2(MSEP) instead of MSEP is used to quantify",
			"\n", "the importance of the variables for predicting a ",
			"quantitative response.", "\n\n", sep="")
		if(standardize)
			warning("In the linear regression case, no standardization should be done.")
	}
	if(is.null(useN)){
		useN<-object$vim$useN
		if(is.null(useN))
			stop("useN needs to be specified when importance = FALSE in logic.bagging.")
	}
	if(object$type==1)
		mat.improve<-compMatImpIndividual1(object,useN=useN,iter=iter,rand=rand)
	else
		mat.improve<-compMatImpIndividual3(object,useN=useN,iter=iter,prob.case=prob.case,
			rand=rand)
	data<-object$data
	varnames<-colnames(data)
	if(standardize)
		vim<-standardizeMatImp(mat.improve,mu=mu)
	else
		vim<-rowMeans(mat.improve)
	names(vim)<-rownames(mat.improve)<-varnames
	if(prop){
		mat.prop<-getVarInTree(object$logreg.model,ncol(data),type=object$type)
		prop<-rowMeans(mat.prop)
		names(prop)<-varnames
	}
	else
		prop<-NULL
	measure<-paste(if(standardize) "Standardized \n", ifelse(is.null(iter),"Removing",
		"Permutation")," Based Individual",sep="")
	if(standardize)
		threshold<-qt(1-0.05/nrow(mat.improve),ncol(mat.improve)-1)
	else{
		if(object$type==2)
			threshold <- qf(1-0.05/nrow(mat.improve), ncol(mat.improve), 
				ncol(mat.improve))
		else
			mu <- threshold <- NULL
	}
	if(!addMatImp)
		mat.improve<-NULL
	vim.out<-list(vim=vim,prop=prop,primes=varnames,type=object$type,param=NULL,
		mat.imp=mat.improve,measure=measure,useN=useN,threshold=threshold,mu=mu,
		iter=iter,name="Variable")
	class(vim.out)<-"logicFS"
	vim.out
}

