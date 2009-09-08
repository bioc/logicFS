vim.logicFS<-function(log.out,useN=TRUE,onlyRemove=FALSE,prob.case=.5,addInfo=FALSE,
		addMatImp=TRUE){
	type<-log.out$type
	if(!type%in%c(1:3,9))
		stop("Currently only available for classification and\n",
			"linear and (multinomial) logistic regression.")
	if(type!=1)
		onlyRemove <- TRUE
	list.primes<-logic.pimp(log.out)
	if(type!=1 | onlyRemove)
		list.primes <- check4NullModels(list.primes)
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
		rmMonoPI<-function(lpi,mono,type){
			if(type==9)
				lapply(lpi,function(x) x[!x%in%mono])
			else
				lpi[!lpi%in%mono]
		}
		for(i in 1:B)
			list.primes[[i]]<-lapply(list.primes[[i]],rmMonoPI,mono=mono,type=type)
	}
	cl<-log.out$cl
	inbagg<-log.out$inbagg
	n.cl<-length(cl)
	mat.imp<-matrix(0,length(vec.primes),B)
	rownames(mat.imp)<-colnames(mat.eval)
	if(type==1){
		vim.fun<-ifelse(onlyRemove,"vim.singleRemove","vim.singleBoth")
		FUN<-match.fun(vim.fun)
		for(i in 1:B){
			oob<-which(!(1:n.cl)%in%inbagg[[i]])
			mat.imp[,i]<-FUN(list.primes[[i]][[1]],mat.eval[oob,],cl[oob],useN=useN)
		}
	}
	if(type==2){
		cat("Note: Since version 1.15.8 log2(MSEP) instead of MSEP is used to quantify",
			"\n", "the importance of the interactions for predicting a ",
			"quantitative response.", "\n\n", sep="")
		list.primes<-check.listprimes(list.primes,log.out$ntrees,B)
		for(i in 1:B)
			mat.imp[,i]<-vim.lm(list.primes[[i]],mat.eval,inbagg[[i]],cl)
	}
	if(type==3){
		list.primes<-check.listprimes(list.primes,log.out$ntrees,B)
		for(i in 1:B)
			mat.imp[,i]<-vim.multiple(list.primes[[i]],mat.eval,inbagg[[i]],cl,
				prob.case=prob.case,useN=useN)
	}
	if(type==9){
		n.reg<-length(levels(cl))-1
		list.primes<-lapply(list.primes,check.listprimes,ntrees=log.out$ntrees,B=n.reg)
		lmodel<-log.out$logreg.model
		for(i in 1:B)
			mat.imp[,i]<-vim.MLR(lmodel[[i]],list.primes[[i]],mat.eval,log.out$data,
				cl,inbagg[[i]],useN=useN)$vec.out
	}
	vim<-rowMeans(mat.imp)
	prop<-prop[vec.primes]
	primes<-getNames(vec.primes,colnames(log.out$data))
	param<-if(addInfo) list(B=B,ntrees=log.out$ntrees,nleaves=log.out$nleaves,sampling=log.out$sampling)
		else NULL
	if(!addMatImp)
		mat.imp<-NULL
	if(type==9)
		measure<-"Multiple Tree"
	else
		measure<-switch(type,"Single Tree","Quantitative Response","Multiple Tree")
	if(type==1 && onlyRemove)
		measure<-paste(measure,"\n (Only Removing)",sep="")
	vim.out<-list(vim=vim,prop=prop,primes=primes,type=type,param=param,mat.imp=mat.imp,
		measure=measure,useN=useN,threshold=NULL,mu=NULL)
	class(vim.out)<-"logicFS"
	vim.out	
}


