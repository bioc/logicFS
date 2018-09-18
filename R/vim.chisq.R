`vim.chisq` <-
function(object,data=NULL,cl=NULL){
	if(!object$type%in%c(1,3))
		stop("Only available for the classification and the logistic regression case.")
	# requireNamespace("siggenes", quietly=TRUE)
	check.out<-checkDataCl(object,data=data,cl=cl)
	vim<-check.out$vim
	mat.eval<-getMatEval(check.out$data,vim$primes)
	mat.eval<-t(mat.eval)
	stat<-siggenes::chisqClass(mat.eval+1,check.out$cl+1,2)
	names(stat)<-names(vim$vim)
	vim$vim<-stat
	vim$measure<-"ChiSquare Based"
	vim$threshold<-qchisq(1-0.05/length(stat),1)
	vim$mu<-NULL
	vim$prop<-NULL
	vim$mat.imp<-NULL
	vim$type<-object$type
	vim
}

