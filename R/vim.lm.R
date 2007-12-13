vim.lm<-function(mprimes,mat.eval,inbagg,cl,prob.case=0.5){
	primes<-unique(unlist(mprimes))
	n.trees<-length(mprimes)
	n.primes<-length(primes)
	oob<-which(!(1:nrow(mat.eval))%in%inbagg)
	if(n.primes==1){
		improve<-vim.lm.oneprime(mat.eval[,primes],cl,oob,prob.case=prob.case)
		names(improve)<-primes
		return(improve)
	}
	mat.in<-cbind(1-diag(n.primes),1)
	rownames(mat.in)<-primes		
	list.eval<-lapply(mprimes,function(x,e=mat.eval,i=mat.in) 
		e[,x,drop=FALSE]%*%i[x,,drop=FALSE]>0)
	le.vec<-n.primes+1
	vec.improve<-numeric(le.vec)
	getIth<-function(x,ids=NULL) x[,ids]
	for(i in 1:le.vec){
		mat.model<-matrix(unlist(lapply(list.eval,getIth,ids=i)),ncol=n.trees)
		if(any(colSums(mat.model)==0))
			mat.model<-mat.model[,colSums(mat.model)>0]
		mat.model<-data.frame(cl=cl,mat.model)
		lm.out<-lm(cl~.,data=mat.model[inbagg,])
		preds<-predict(lm.out,mat.model[oob,])
		vec.improve[i]<-mean((preds-cl[oob])^2)
	}
	vec.improve<-vec.improve-vec.improve[le.vec]
	vec.improve<-vec.improve[-le.vec]
	names(vec.improve)<-primes
	vec.improve
}

vim.lm.oneprime<-function(oneprime,cl,oob,prob.case=0.5){
	beta0<-mean(cl[-oob])
	rss.null<-mean((beta0-cl[oob])^2)
	dat<-data.frame(cl=cl,x=oneprime)
	lm.out<-lm(cl~.,data=dat[-oob,])
	preds<-predict(glm.out,dat[oob,])
	rss.prime<-mean((preds-cl[oob])^2)
	rss.null-rss.prime
}



