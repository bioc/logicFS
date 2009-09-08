vim.lm<-function(mprimes,mat.eval,inbagg,cl){
	primes<-unique(unlist(mprimes))
	n.trees<-length(mprimes)
	n.primes<-length(primes)
	oob<-which(!(1:nrow(mat.eval))%in%inbagg)
	vec.out<-numeric(ncol(mat.eval))
	names(vec.out)<-colnames(mat.eval)
	if(n.primes==1){
		vec.out[primes]<-vim.lm.oneprime(mat.eval[,primes],cl,oob)
		return(vec.out)
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
	vec.improve <- log2(vec.improve) - log2(vec.improve[le.vec])
	vec.out[primes]<-vec.improve[-le.vec]
	vec.out
}

vim.lm.oneprime<-function(oneprime,cl,oob){
	beta0<-mean(cl[-oob])
	rss.null<-mean((beta0-cl[oob])^2)
	dat<-data.frame(cl=cl,x=oneprime)
	lm.out<-lm(cl~.,data=dat[-oob,])
	preds<-predict(lm.out,dat[oob,])
	rss.prime<-mean((preds-cl[oob])^2)
	log2(rss.null) - log2(rss.prime)
}



