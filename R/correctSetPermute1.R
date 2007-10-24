`correctSetPermute1` <-
function(tree,newdata,newcl,set,n.var,n.set,iter){
	treePreds<-eval.logreg(tree,newdata)
	if(any(treePreds>1))
		stop("Some treePreds > 1. Please inform the author about this error.")
	cortree<-sum(treePreds==newcl)
	obs<-1:nrow(newdata)
	vec.preds<-numeric(iter)
	corpreds<-numeric(n.set)
	for(i in 1:n.set){
		tmpdata<-newdata
		tmpvar<-set[[i]]
		for(j in 1:iter){
			tmpids<-sample(obs)
			tmpdata[,tmpvar]<-newdata[tmpids,tmpvar]
			preds<-eval.logreg(tree,tmpdata)
			vec.preds[j]<-sum(preds==newcl)
		}
		corpreds[i]<-cortree-mean(vec.preds)
	}
	corpreds
}

