logic.pimp<-function(log.out){
	if(!is(log.out,"logicBagg"))
		stop("log.out must be an object of class logicBagg.")
	lmodel<-log.out$logreg.model
	if(log.out$type==9){
		n.reg<-length(lmodel[[1]])
		list.primes<-lapply(lmodel,pimpMLR,n.reg=n.reg)
		return(list.primes)
	}
	n.lmodel<-length(lmodel)
	list.primes<-vector("list",n.lmodel)
	for(i in 1:n.lmodel)
		list.primes[[i]]<-lapply(lmodel[[i]]$trees,getPImps,type=log.out$type)
	list.primes
}

pimpMLR<-function(lreg,n.reg){
	out<-vector("list",n.reg)
	for(i in 1:n.reg)
		out[[i]]<-lapply(lreg[[i]]$trees,getPImps,type=9)
	out
}

