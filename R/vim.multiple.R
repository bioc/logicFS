vim.multiple<-function(mprimes,mat.eval,inbagg,cl,prob.case=0.5){
	primes<-unique(unlist(mprimes))
	n.trees<-length(mprimes)
	n.primes<-length(primes)
	oob<-which(!(1:nrow(mat.eval))%in%inbagg)
	if(n.primes==1){
		improve<-vim.multiple.oneprime(mat.eval[,primes],cl,oob,prob.case=prob.case)
		names(improve)<-primes
		return(improve)
	}
	mat.in<-cbind(1-diag(n.primes),1)
	rownames(mat.in)<-primes		
	list.eval<-lapply(mprimes,function(x,e=mat.eval,i=mat.in) 
		e[,x,drop=FALSE]%*%i[x,,drop=FALSE]>0)
	vec.improve<-numeric(n.primes+1)
	getIth<-function(x,ids=NULL) x[,ids]
	for(i in 1:(n.primes+1)){
		mat.model<-matrix(unlist(lapply(list.eval,getIth,ids=i)),ncol=n.trees)
		if(any(colSums(mat.model)==0))
			mat.model<-mat.model[,colSums(mat.model)>0]
		mat.model<-data.frame(cl=cl,mat.model)
		glm.out<-glm(cl~.,data=mat.model[inbagg,],family="binomial")
		preds<-predict(glm.out,mat.model[oob,],type="response")>prob.case
		vec.improve[i]<-sum(preds==cl[oob])
	}
	vec.improve<-vec.improve[n.primes+1]-vec.improve
	vec.improve<-vec.improve[-(n.primes+1)]
	names(vec.improve)<-primes
	vec.improve
}

