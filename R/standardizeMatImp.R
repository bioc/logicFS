`standardizeMatImp` <-
function(mat.imp,mu=0){
	require(genefilter)
	if(mu<0)
		stop("mu must be non-negative.",call.=FALSE)
	mat.imp<-mat.imp-mu
	stat<-rowttests(mat.imp)$statistic
	stat[is.infinite(stat)]<-NA
	stat
}

