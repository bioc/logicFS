logic.pimp<-function(log.out){
	if(!is(log.out,"logicBagg"))
		stop("log.out must be an object of class logicBagg.")
	lmodel<-log.out$logreg.model
	n.lmodel<-length(lmodel)
	list.primes<-vector("list",n.lmodel)
	for(i in 1:n.lmodel)
		list.primes[[i]]<-lapply(lmodel[[i]]$trees,getPImps,type=log.out$type)
	list.primes
}

