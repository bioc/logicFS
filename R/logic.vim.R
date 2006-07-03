logic.vim<-function(log.out,prob.case=.5,addInfo=FALSE){
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
	attach(data,warn.conflicts=FALSE)
	mat.eval<-sapply(vec.primes,function(x) eval(parse(text=x)))
	detach(data)
	cl<-log.out$cl
	inbagg<-log.out$inbagg
	n.cl<-length(cl)
	mat.imp<-matrix(0,B,length(vec.primes))
	colnames(mat.imp)<-colnames(mat.eval)
	if(type==1){
		for(i in 1:B){
			oob<-which(!(1:n.cl)%in%inbagg[[i]])
			mat.imp[i,]<-vim.single(list.primes[[i]][[1]],mat.eval[oob,],
				cl[oob])
		}
	}
	else{
		for(i in 1:B){
			tmp.imp<-vim.multiple(list.primes[[i]],mat.eval,inbagg[[i]],
				cl=cl,prob.case=prob.case)
			mat.imp[i,names(tmp.imp)]<-tmp.imp
		}
	}
	vim<-colMeans(mat.imp)
	prop<-prop[vec.primes]
	primes<-getNames(vec.primes,colnames(log.out$data))
	param<-if(addInfo) list(B=B,ntrees=log.out$ntrees,nleaves=log.out$nleaves)
		else NULL
	vim.out<-list(vim=vim,prop=prop,primes=primes,type=type,param=param)
	class(vim.out)<-"logicFS"
	vim.out	
}

