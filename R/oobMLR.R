oobMLR<-function(models,x,inbagg,cl){
	levs<-levels(cl)
	n.lev<-length(levs)
	n.obs<-nrow(x)
	mat.prob<-matrix(0,n.obs,n.lev)
	for(i in 1:length(models)){
		oob<-which(!(1:n.obs)%in%inbagg[[i]])
		tmp.mat<-compMatProbMLR(models[[i]],x[oob,],n.lev)
		mat.prob[oob,]<-mat.prob[oob,]+tmp.mat
	}
	rs<-rowSums(mat.prob)
	if(any(rs==0))
		warning(sum(rs==0)," of the observations are in none of the oob samples.")
	ids<-max.col(mat.prob)
	preds<-levs[ids]
	preds[rs==0]<-NA
	mean(preds!=cl,na.rm=TRUE)
}

