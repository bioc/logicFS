`correctSetRemove1` <-
function(tree,newdata,newcl,set,n.var,n.set){
	treePreds<-eval.logreg(tree,newdata)
	if(any(treePreds>1))
		stop("Some treePreds > 1. Please inform the author about this error.")
	cortree<-sum(treePreds==newcl)	
	corpreds<-numeric(n.set)
	newdata<-cbind(newdata,1,0)
	for(i in 1:n.set){
		newtree<-getNewTree(tree,set[[i]],n.var)
		newtree<-checkNewTree(newtree,n.var)
		if(is.null(newtree))
			corpreds[i]<-cortree-sum(newcl==0)
		else{
			tmp<-eval.logreg(newtree,newdata)
			if(any(tmp>1))
				stop("Some preds > 1. Please inform the author about this error.")
			corpreds[i]<-cortree-sum(newcl==tmp)
		}
	}
	corpreds
}

