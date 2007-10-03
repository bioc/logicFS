`vim.ebam` <-
function(object,data=NULL,cl=NULL,nameEBAM=NULL,...){
	require(siggenes)
	check.out<-checkDataCl(object,data=data,cl=cl)
	vim<-check.out$vim
	mat.eval<-getMatEval(check.out$data,vim$primes)
	mat.eval<-t(mat.eval)
	rownames(mat.eval)<-names(vim$vim)
	ebam.out<-ebam(mat.eval+1,check.out$cl+1,method=cat.ebam,...)
	if(!is.null(nameEBAM))
		assign(nameEBAM,ebam.out,envir=.GlobalEnv)
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
	vim$type<-NULL
	vim
}
