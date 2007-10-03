vim.logicFS<-function(log.out,prob.case=.5,addInfo=FALSE,addMatImp=TRUE){
	type<-log.out$type
	if(!type%in%c(1,3))
		stop("Currently only available for classification and logistic regression.")
	list.primes<-logic.pimp(log.out)
	B<-length(list.primes)
	if(type==1)
		vec.primes<-unlist(list.primes)
	else
		vec.primes<-unlist(lapply(list.primes,function(x) unique(unlist(x))))
	prop<-table(vec.primes)/B
	vec.primes<-unique(vec.primes)
	data<-as.data.frame(log.out$data)
	colnames(data)<-paste("X",1:ncol(data),sep="")
	mat.eval<-getMatEval(data,vec.primes,check=FALSE)
	if(ncol(mat.eval)<length(vec.primes)){
		ids<-which(!vec.primes%in%colnames(mat.eval))
		mono<-vec.primes[ids]
		vec.primes<-vec.primes[-ids]
		for(i in 1:B)
			list.primes[[i]]<-lapply(list.primes[[i]],function(x) x[!x%in%mono])
	}
	cl<-log.out$cl
	inbagg<-log.out$inbagg
	n.cl<-length(cl)
	mat.imp<-matrix(0,length(vec.primes),B)
	rownames(mat.imp)<-colnames(mat.eval)
	if(type==1){
		for(i in 1:B){
			oob<-which(!(1:n.cl)%in%inbagg[[i]])
			mat.imp[,i]<-vim.single(list.primes[[i]][[1]],mat.eval[oob,],
				cl[oob])
		}
	}
	else{
		for(i in 1:B){
			list.primes<-check.listprimes(list.primes,log.out$ntrees,B)
			tmp.imp<-vim.multiple(list.primes[[i]],mat.eval,inbagg[[i]],
				cl=cl,prob.case=prob.case)
			mat.imp[names(tmp.imp),i]<-tmp.imp
		}
	}
	vim<-rowMeans(mat.imp)
	prop<-prop[vec.primes]
	primes<-getNames(vec.primes,colnames(log.out$data))
	param<-if(addInfo) list(B=B,ntrees=log.out$ntrees,nleaves=log.out$nleaves,sampling=log.out$sampling)
		else NULL
	if(!addMatImp)
		mat.imp<-NULL
	measure<-ifelse(type==1,"Single Tree","Multiple Tree")
	vim.out<-list(vim=vim,prop=prop,primes=primes,type=type,param=param,mat.imp=mat.imp,
		measure=measure,threshold=NULL,mu=NULL)
	class(vim.out)<-"logicFS"
	vim.out	
}

