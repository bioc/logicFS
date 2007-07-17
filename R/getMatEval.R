getMatEval<-function(data,vec.primes,check=TRUE){
	if(!is.data.frame(data))
		stop("data must be a data frame.")
	if(check){
		tmp<-gsub("!","",vec.primes)
		tmp<-unlist(strsplit(tmp," & "))
		tmp<-unique(tmp)
		if(any(!tmp%in%colnames(data)))
			stop("Some of the variables used in vec.primes are not in data.")
	}
	attach(data,warn.conflicts=FALSE)
	mat.eval<-sapply(vec.primes,function(x) eval(parse(text=x)))
	detach(data)
	cs<-colSums(mat.eval)
	if(any(cs%in%c(0,nrow(mat.eval)))){
		ids<-which(cs%in%c(0,nrow(mat.eval)))
		warning("For ",length(ids)," of the ",ncol(mat.eval)," interactions, ",
			"all observations have the same value.\n","These interactions are thus removed.",
			call.=FALSE)
		mat.eval<-mat.eval[,-ids]
	}
	return(mat.eval)
}




