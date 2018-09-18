`standardizeMatImp` <-
function(mat.imp,mu=0){
	# requireNamespace("genefilter", quietly=TRUE)
	if(mu<0)
		stop("mu must be non-negative.",call.=FALSE)
	mat.imp<-mat.imp-mu
	stat<-genefilter::rowttests(mat.imp)$statistic
	stat[is.infinite(stat)]<-NA
	stat
}

