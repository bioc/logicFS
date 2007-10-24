`correctPredsRemove1` <-
function(tree,newdata,newcl,n.var,knot,cortree){
	corpreds<-numeric(n.var)
	if(length(knot)==1){
		corpreds[knot]<-cortree-sum(newcl==0)
		return(corpreds)
	}
	newdata<-cbind(newdata,1,0)
	for(i in knot){
		newtree<-getNewTree(tree,i,n.var)
		tmp<-eval.logreg(newtree,newdata)
		if(any(tmp>1))
			stop("Some preds > 1. Please inform the author about this error.")
		corpreds[i]<-cortree-sum(newcl==tmp)
	}
	corpreds
}

