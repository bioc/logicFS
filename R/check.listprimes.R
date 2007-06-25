check.listprimes<-function(lp,ntrees,B){
	any.null<-function(x) any(unlist(lapply(x,is.null)))
	vec.null<-unlist(lapply(lp,any.null))
	if(any(vec.null)){
		warning(sum(vec.null)," of the ",B," logic regression models ",
			ifelse(sum(vec.null)==1,"is","are")," composed of less than ",
			ntrees," logic trees.",call.=FALSE)
		ids<-which(vec.null)
		for(i in ids){
			tmp<-lp[[i]]
			tmpids<-which(!unlist(lapply(tmp,is.null)))
			lp[[i]]<-tmp[tmpids]
		}
	}
	lp
}


