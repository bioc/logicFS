vim.snp <- function(object, useN=NULL, iter=NULL, standardize=NULL, mu=0, 
		addMatImp=FALSE, prob.case=0.5, rand=NA){
	out <- vim.set(object, useN=useN, iter=iter, standardize=standardize, mu=mu,
		addMatImp=addMatImp, prob.case=prob.case, rand=rand)
	out$measure <- gsub("Set", "SNP", out$measure)
	out$name <- "SNP"
	out
}


`vim.set` <-
function(object,set=NULL,useN=NULL,iter=NULL,standardize=NULL,mu=0,addMatImp=FALSE,
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
			"\n", "the importance of the (sets of) SNPs for predicting a ",
			"quantitative response.", "\n\n", sep="")
		if(standardize)
			warning("In the linear regression case, no standardization should be done.")
	}
	cn<-colnames(object$data)
	n.var<-ncol(object$data)
	set<-checkSet(set,n.var,cn)
	if(is.null(useN)){
		useN<-object$vim$useN
		if(is.null(useN))
			stop("useN needs to be specified when importance = FALSE in logic.bagging.")
	}
	if(object$type==1)
		mat.improve<-compMatImpSet1(object,set,useN=useN,iter=iter,rand=rand)
	else
		mat.improve<-compMatImpSet3(object,set,useN=useN,iter=iter,prob.case=prob.case,
			rand=rand)
	if(standardize)
		vim<-standardizeMatImp(mat.improve,mu=mu)
	else
		vim<-rowMeans(mat.improve)
	names(vim)<-rownames(mat.improve)<-names(set)
	measure<-paste(if(standardize) "Standardized \n", ifelse(is.null(iter),"Removing","Permutation"),
		"Based Set")
	if(standardize)
		threshold<-qt(1-0.05/nrow(mat.improve),ncol(mat.improve)-1)
	else{
		if(object$type==2)
			threshold <- qf(1-0.05/nrow(mat.improve), ncol(mat.improve),
				ncol(mat.improve))
		else
			threshold <- mu <- NULL
	}
	if(!addMatImp)
		mat.improve<-NULL
	vim.out<-list(vim=vim,prop=NULL,primes=names(set),type=object$type,param=NULL,mat.imp=mat.improve,
		measure=measure,useN=useN,threshold=threshold,mu=mu,iter=iter,name="Set")
	class(vim.out)<-"logicFS"
	vim.out
}

