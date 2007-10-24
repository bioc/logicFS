`correctPredsRemove3` <-
function(ltree,olddata,newdata,oldcl,newcl,n.var,prob.case=0.5){
	corpreds<-numeric(n.var)
	treePreds<-predict(ltree,newdata,3)>prob.case
	cortree<-sum(treePreds==newcl)
	knot<-getKnots(ltree$trees)
	newdata<-cbind(newdata,1,0)
	olddata<-cbind(olddata,1,0)
	for(i in knot){
		newtree<-lapply(ltree$trees,getNewTree,i,n.var)
		ids<-!unlist(lapply(newtree,is.null))
		newtree<-newtree[ids]
		mat.design<-sapply(newtree,eval.logreg,olddata)
		mat.design<-cbind(1,mat.design)
		coef<-glm.fit(mat.design,oldcl,family=binomial())$coefficients
		mat.new<-sapply(newtree,eval.logreg,newdata)
		mat.new<-cbind(1,mat.new)
		preds<-as.vector(mat.new%*%coef)
		preds <- exp(preds)/(1 + exp(preds))
		if(any(preds>1 | preds<0))
			stop("Something went wrong.")
		preds<-preds>prob.case
		corpreds[i]<-cortree-sum(newcl==preds)
	}		
	corpreds	
}

