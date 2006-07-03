generateTruthTab<-function(ltree){
	if(!is(ltree,"logregtree"))
		stop("ltree must be an object of class logregtree.")
	model.var<-ltree$trees[,3]
	model.var<-sort(model.var[model.var!=0])
	model.var<-model.var[!duplicated(model.var)]
	mat.perms<-getPerms(length(model.var))
	colnames(mat.perms)<-paste("X",model.var,sep="")
	mat.bin<-matrix(0,nrow(mat.perms),max(model.var))
	mat.bin[,model.var]<-mat.perms
	pred.out<-eval.logreg(ltree,mat.bin)
	mat.truth<-cbind(mat.perms,outcome=pred.out)
	mat.truth
}

