`correctSetRemove3` <-
function(ltree,olddata,newdata,oldcl,newcl,set,n.var,n.set,prob.case=0.5){
	treePreds<-predict(ltree,newdata,3)>prob.case
	cortree<-sum(treePreds==newcl)
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
		coef<-glm.fit(mat.design,oldcl,family=binomial())$coefficients
		if(any(is.na(coef)))
			coef[is.na(coef)]<-0
		preds<-as.vector(mat.new%*%coef)
		preds<-exp(preds)/(1+exp(preds))
		if(any(preds>1 | preds<0))
			stop("Something went wrong.")
		preds<-preds>prob.case
		corpreds[i]<-cortree-sum(newcl==preds)
	}
	corpreds
}

