correctPredsRemove9<-function(ltree,olddata,newdata,oldcl,newcl,n.var,prob.case=NULL){
	corpreds<-numeric(n.var)
	levs<-levels(oldcl)
	n.lev<-length(levs)
	mat.prob<-matrix(0,length(newcl),n.lev)
	for(i in 2:n.lev)
		mat.prob[,i]<-predict(ltree[[i-1]],newdata,9)
	treePreds<-max.col(mat.prob)
	treePreds<-levs[treePreds]
	cortree<-sum(treePreds==newcl)
	matKnots<-getMatKnots(ltree,n.var,n.lev-1)
	knot<-which(rowSums(matKnots)>0)
	newdata<-cbind(newdata,1,0)
	olddata<-cbind(olddata,1,0)
	for(i in knot){
		ids<-which(matKnots[i,]==1)
		tmp.prob<-mat.prob
		for(j in ids){
			newtree<-lapply(ltree[[j]]$trees,getNewTree,i,n.var)
			ids2<-!unlist(lapply(newtree,is.null))
			newtree<-newtree[ids2]
			idsIn<-oldcl%in%levs[c(1,j+1)]
			y<-(oldcl[idsIn]==levs[j+1])*1
			mat.design<-sapply(newtree,eval.logreg,olddata[idsIn,])
			mat.design<-cbind(1,mat.design)
			coef<-glm.fit(mat.design,y,family=binomial())$coefficients
			if(any(is.na(coef)))
				coef[is.na(coef)]<-0
			mat.new<-sapply(newtree,eval.logreg,newdata)
			mat.new<-cbind(1,mat.new)
			tmp.prob[,j+1]<-mat.new%*%coef
		}
		preds<-max.col(tmp.prob)
		preds<-levs[preds]
		corpreds[i]<-cortree-sum(preds==newcl)
	}
	corpreds
}


getMatKnots<-function(ltree,n.var,n.reg){
	mat<-matrix(0,n.var,n.reg)
	for(i in 1:n.reg){
		ids<-getKnots(ltree[[i]]$trees)
		mat[ids,i]<-1
	}
	mat
}


correctPredsPermute9<-function(ltree,newdata,newcl,n.var,iter,prob.case=NULL){
	corpreds<-numeric(n.var)
	levs<-levels(newcl)
	n.lev<-length(levs)
	mat.prob<-matrix(0,length(newcl),n.lev)
	for(i in 2:n.lev)
		mat.prob[,i]<-predict(ltree[[i-1]],newdata,9)
	treePreds<-max.col(mat.prob)
	treePreds<-levs[treePreds]
	cortree<-sum(treePreds==newcl)
	matKnots<-getMatKnots(ltree,n.var,n.lev-1)
	knot<-which(rowSums(matKnots)>0)
	vec.preds<-numeric(iter)
	for(i in knot){
		ids<-which(matKnots[i,]==1)
		tmp.prob<-mat.prob
		tmpdata<-newdata
		obsval<-newdata[,i]
		for(j in 1:iter){
			tmpdata[,i]<-sample(obsval)
			for(k in ids)
				tmp.prob[,k+1]<-predict(ltree[[k]],tmpdata,9)
			preds<-max.col(tmp.prob)
			preds<-levs[preds]
			vec.preds[j]<-sum(preds==newcl)
		}
		corpreds[i]<-cortree-mean(vec.preds)
	}
	corpreds
}


getMatSets<-function(ltree,set,n.var,n.reg){
	mat.knots<-getMatKnots(ltree,n.var,n.reg)
	mat<-matrix(0,length(set),n.var)
	for(i in 1:length(set))
		mat[i,set[[i]]]<-1
	mat%*%mat.knots
}


correctSetRemove9<-function(ltree,olddata,newdata,oldcl,newcl,set,n.var,n.set,prob.case=NULL){
	corpreds<-numeric(n.set)
	levs<-levels(oldcl)
	n.lev<-length(levs)
	mat.prob<-matrix(0,length(newcl),n.lev)
	for(i in 2:n.lev)
		mat.prob[,i]<-predict(ltree[[i-1]],newdata,9)
	treePreds<-max.col(mat.prob)
	treePreds<-levs[treePreds]
	cortree<-sum(treePreds==newcl)
	matSets<-getMatSets(ltree,set,n.var,n.lev-1)
	whichSets<-which(rowSums(matSets)>0)
	newdata<-cbind(newdata,1,0)
	olddata<-cbind(olddata,1,0)
	for(i in whichSets){
		ids<-which(matSets[i,]>0)
		tmp.prob<-mat.prob
		for(j in ids){
			newtree<-lapply(ltree[[j]]$trees,getNewTree,set[[i]],n.var)
			newtree<-lapply(newtree,checkNewTree,n.var)
			ids2<-!unlist(lapply(newtree,is.null))
			idsIn<-oldcl%in%levs[c(1,j+1)]
			y<-(oldcl[idsIn]==levs[j+1])*1
			if(sum(ids2)==0){
				mat.design<-matrix(1,nrow=length(idsIn))
				mat.new<-matrix(1,nrow=nrow(newdata))
			}
			else{
				newtree<-newtree[ids2]
				mat.design<-sapply(newtree,eval.logreg,olddata[idsIn,])
				mat.design<-cbind(1,mat.design)
				mat.new<-sapply(newtree,eval.logreg,newdata)
				mat.new<-cbind(1,mat.new)
			}
			coef<-glm.fit(mat.design,y,family=binomial())$coefficients
			if(any(is.na(coef)))
				coef[is.na(coef)]<-0
			tmp.prob[,j+1]<-mat.new%*%coef
		}
		preds<-max.col(tmp.prob)
		preds<-levs[preds]
		corpreds[i]<-cortree-sum(preds==newcl)
	}
	corpreds
}

			
correctSetPermute9<-function(ltree,newdata,newcl,set,n.var,n.set,iter,prob.case=NULL){
	levs<-levels(newcl)
	n.lev<-length(levs)
	mat.prob<-matrix(0,length(newcl),n.lev)
	for(i in 2:n.lev)
		mat.prob[,i]<-predict(ltree[[i-1]],newdata,9)
	treePreds<-max.col(mat.prob)
	treePreds<-levs[treePreds]
	cortree<-sum(treePreds==newcl)
	matSets<-getMatSets(ltree,set,n.var,n.lev-1)
	whichSets<-which(rowSums(matSets)>0)
	obs<-1:nrow(newdata)
	corpreds<-numeric(n.set)
	vec.preds<-numeric(iter)
	for(i in whichSets){
		ids<-which(matSets[i,]>0)
		tmp.prob<-mat.prob
		tmpdata<-newdata
		tmpvar<-set[[i]]
		for(j in 1:iter){
			tmpids<-sample(obs)
			tmpdata[,tmpvar]<-newdata[tmpids,tmpvar]
			for(k in ids)
				tmp.prob[,k+1]<-predict(ltree[[k]],tmpdata,9)
			preds<-max.col(tmp.prob)
			preds<-levs[preds]
			vec.preds[j]<-sum(preds==newcl)
		}
		corpreds[i]<-cortree-mean(vec.preds)
	}
	corpreds
}
			


