`correctSetPermute3` <-
function(ltree,newdata,newcl,set,n.var,n.set,iter,prob.case=0.5){
	treePreds<-predict(ltree,newdata,3)>prob.case
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
			preds<-predict(ltree,tmpdata,3)>prob.case
			vec.preds[j]<-sum(preds==newcl)
		}
		corpreds[i]<-cortree-mean(vec.preds)
	}
	corpreds
}

