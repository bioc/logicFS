`correctPredsPermute3` <-
function(ltree,newdata,newcl,n.var,iter,prob.case=0.5){
	corpreds<-numeric(n.var)
	treePreds<-predict(ltree,newdata,3)>prob.case
	cortree<-sum(treePreds==newcl)
	knot<-getKnots(ltree$trees)
	vec.preds<-numeric(iter)
	for(i in knot){
		tmpdata<-newdata
		obsval<-newdata[,i]
		for(j in 1:iter){
			tmpdata[,i]<-sample(obsval)
			preds<-predict(ltree,tmpdata,3)>prob.case
			vec.preds[j]<-sum(preds==newcl)
		}
		corpreds[i]<-cortree-mean(vec.preds)
	}
	corpreds
}

