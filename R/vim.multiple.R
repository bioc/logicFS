vim.multiple<-function(mprimes,mat.eval,inbagg,cl,prob.case=0.5,useN=TRUE){
	primes<-unique(unlist(mprimes))
	n.trees<-length(mprimes)
	n.primes<-length(primes)
	oob<-which(!(1:nrow(mat.eval))%in%inbagg)
	vec.out<-numeric(ncol(mat.eval))
	names(vec.out)<-colnames(mat.eval)
	if(n.primes==1){
		vec.out[primes]<-vim.multiple.oneprime(mat.eval[,primes],cl,oob,
			prob.case=prob.case)
		if(!useN)
			vec.out<-vec.out/length(oob)
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
		glm.out<-glm(cl~.,data=mat.model[inbagg,],family="binomial")
		preds<-predict(glm.out,mat.model[oob,],type="response")>prob.case
		vec.improve[i]<-sum(preds==cl[oob])
	}
	vec.improve<-vec.improve[le.vec]-vec.improve
	vec.out[primes]<-vec.improve[-le.vec]
	if(!useN)
		vec.out<-vec.out/length(oob)
	vec.out
}

