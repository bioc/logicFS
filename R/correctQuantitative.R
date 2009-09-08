`correctPredsRemove2` <-
function(ltree,olddata,newdata,oldcl,newcl,n.var,prob.case=NULL){
	corpreds<-numeric(n.var)
	treePreds<-predict(ltree,newdata,2)
	cortree <- log2(mean((treePreds-newcl)^2))
	knot<-getKnots(ltree$trees)
	newdata<-cbind(newdata,1,0)
	olddata<-cbind(olddata,1,0)
	for(i in knot){
		newtree<-lapply(ltree$trees,getNewTree,i,n.var)
		ids<-!unlist(lapply(newtree,is.null))
		newtree<-newtree[ids]
		mat.design<-sapply(newtree,eval.logreg,olddata)
		mat.design<-cbind(1,mat.design)
		coef<-lm.fit(mat.design,oldcl)$coefficients
		if(any(is.na(coef)))
			coef[is.na(coef)]<-0
		mat.new<-sapply(newtree,eval.logreg,newdata)
		mat.new<-cbind(1,mat.new)
		preds<-as.vector(mat.new%*%coef)
		if(any(is.na(preds)))
			stop("Something went wrong.")
		corpreds[i] <- log2(mean((newcl-preds)^2)) - cortree
	}		
	corpreds	
}


`correctPredsPermute2` <-
function(ltree,newdata,newcl,n.var,iter,prob.case=0.5){
	corpreds<-numeric(n.var)
	treePreds<-predict(ltree,newdata,2)
	cortree <- log2(mean((treePreds-newcl)^2))
	knot<-getKnots(ltree$trees)
	vec.preds<-numeric(iter)
	for(i in knot){
		tmpdata<-newdata
		obsval<-newdata[,i]
		for(j in 1:iter){
			tmpdata[,i]<-sample(obsval)
			preds<-predict(ltree,tmpdata,2)
			vec.preds[j] <- log2(mean((preds-newcl)^2))
		}
		corpreds[i]<-mean(vec.preds)-cortree
	}
	corpreds
}


`correctSetRemove2` <-
function(ltree,olddata,newdata,oldcl,newcl,set,n.var,n.set,prob.case=0.5){
	treePreds<-predict(ltree,newdata,2)
	cortree <- log2(mean((treePreds-newcl)^2))
	newdata<-cbind(newdata,1,0)
	olddata<-cbind(olddata,1,0)
	corpreds<-numeric(n.set)
	for(i in 1:n.set){
		newtree<-lapply(ltree$trees,getNewTree,set[[i]],n.var)
		newtree<-lapply(newtree,checkNewTree,n.var)
		ids<-!unlist(lapply(newtree,is.null))
		if(sum(ids)==0){
			mat.design<-matrix(1,nrow=nrow(olddata))
			mat.new<-matrix(1,nrow=nrow(newdata))
		}
		else{
			newtree<-newtree[ids]
			mat.design<-sapply(newtree,eval.logreg,olddata)
			mat.design<-cbind(1,mat.design)
			mat.new<-sapply(newtree,eval.logreg,newdata)
			mat.new<-cbind(1,mat.new)
		}
		coef<-lm.fit(mat.design,oldcl)$coefficients
		if(any(is.na(coef)))
			coef[is.na(coef)]<-0
		preds<-as.vector(mat.new%*%coef)
		if(any(is.na(preds)))
			stop("Something went wrong.")
		corpreds[i] <- log2(mean((newcl-preds)^2)) - cortree
	}
	corpreds
}


`correctSetPermute2` <-
function(ltree,newdata,newcl,set,n.var,n.set,iter,prob.case=0.5){
	treePreds<-predict(ltree,newdata,2)
	cortree <- log2(mean((treePreds-newcl)^2))
	obs<-1:nrow(newdata)
	vec.preds<-numeric(iter)
	corpreds<-numeric(n.set)
	for(i in 1:n.set){
		tmpdata<-newdata
		tmpvar<-set[[i]]
		for(j in 1:iter){
			tmpids<-sample(obs)
			tmpdata[,tmpvar]<-newdata[tmpids,tmpvar]
			preds<-predict(ltree,tmpdata,2)
			vec.preds[j] <- log2(mean((preds-newcl)^2))
		}
		corpreds[i]<-mean(vec.preds)-cortree
	}
	corpreds
}


