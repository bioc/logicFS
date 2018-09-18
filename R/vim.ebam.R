`vim.ebam` <-
function(object,data=NULL,cl=NULL,storeEBAM=FALSE,...){
	if(!object$type%in%c(1,3))
		stop("Only available for the classification and the logistic regression case.")
	# requireNamespace("siggenes", quietly=TRUE)
	check.out<-checkDataCl(object,data=data,cl=cl)
	vim<-check.out$vim
	mat.eval<-getMatEval(check.out$data,vim$primes)
	mat.eval<-t(mat.eval)
	rownames(mat.eval)<-names(vim$vim)
	ebam.out<-siggenes::ebam(mat.eval+1,check.out$cl+1,method="cat.ebam",...)
	vim$vim<-ebam.out@posterior
	mat.fdr<-ebam.out@mat.fdr
	if(nrow(mat.fdr)>1)
		warning("More than one value for Delta available. Only the first is stored as threshold.",
			call.=TRUE)
	vim$threshold<-mat.fdr[1]
	vim$mu<-NULL
	vim$measure<-"EBAM Based"
	vim$prop<-NULL
	vim$mat.imp<-NULL
	vim$type<-object$type
	if(storeEBAM)
		vim$ebam <- ebam.out
	vim
}

