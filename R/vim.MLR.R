vim.MLR<-function(lmodel,lpi,mat.eval,mat.data,cl,inbagg,useN=TRUE){
	levs<-levels(cl)
	n.lev<-length(levs)
	oob<-which(!(1:length(cl))%in%inbagg)
	mat.data<-mat.data[oob,]
	getME<-function(x,mat) mat[,x,drop=FALSE]
	vec.out<-numeric(ncol(mat.eval))
	names(vec.out)<-colnames(mat.eval)
	mat.prob<-matrix(0,length(oob),n.lev)
	listMat<-vector("list",n.lev-1)
	for(i in 2:n.lev){
		mat.prob[,i]<-predict(lmodel[[i-1]],mat.data,2)
		listMat[[i-1]]<-lapply(lpi[[i-1]],getME,mat=mat.eval)
	}
	rm(mat.eval,mat.data)
	mat.prob<-exp(mat.prob)
	preds<-max.col(mat.prob)
	preds<-levs[preds]
	n.corr<-sum(cl[oob]==preds)
	mat.primes<-getMatPrime(lpi)
	primes<-rownames(mat.primes)
	n.primes<-length(primes)
	vec.improve<-numeric(n.primes)
	for(i in 1:n.primes){
		ids<-which(mat.primes[i,])
		tmp.prob<-mat.prob
		for(j in ids)
			tmp.prob[,j+1]<-getNewProbsMLR(listMat[[j]],inbagg,oob,cl[inbagg],
				primes[i],levs[c(1,j+1)])
		preds<-max.col(tmp.prob)
		preds<-levs[preds]
		vec.improve[i]<-sum(cl[oob]==preds)
	}
	vec.out[primes]<-n.corr-vec.improve
	if(!useN)
		vec.out<-vec.out/length(oob)
	vec.out
	names(vec.improve)<-primes
	list(n.corr=n.corr,vec.improve=vec.improve,vec.out=vec.out)
}


getNewProbsMLR<-function(mats,inbagg,oob,cl.train,prime,vec.lev){
	rS2<-function(x,prime) rowSums(x[,colnames(x)!=prime,drop=FALSE])>0
	tmp<-lapply(mats,rS2,prime=prime)
	mat.model<-matrix(unlist(tmp),ncol=length(mats))
	cs<-colSums(mat.model)
	if(any(cs%in%c(0,nrow(mat.model))))
		mat.model<-mat.model[,!cs%in%c(0,nrow(mat.model)),drop=FALSE]
	mat.model<-cbind(1,mat.model)
	ids<-cl.train%in%vec.lev
	y<-(cl.train[ids]==vec.lev[2])*1
	x<-mat.model[inbagg,]
	coef<-glm.fit(x=x[ids,],y=y,family=binomial())$coef
	mat.model[oob,]%*%coef
}
	
		
getMatPrime<-function(lpi){
	primes<-unique(unlist(lpi))
	listPI<-lapply(lpi,function(x) unique(unlist(x)))
	mat<-sapply(listPI,function(x,y) y%in%x, y=primes)
	rownames(mat)<-primes
	mat
}



