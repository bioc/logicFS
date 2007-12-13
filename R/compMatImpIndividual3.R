`compMatImpIndividual3` <-
function(object,iter=NULL,prob.case=0.5,rand=NA){
	inbagg<-object$inbagg
	ltree<-object$logreg.model
	data<-object$data
	cl<-object$cl
	n.var<-ncol(data)
	n.obs<-nrow(data)
	B<-length(ltree)
	mat.improve<-matrix(0,n.var,B)
	tmp.fun<-paste("correctPreds",ifelse(is.null(iter),"Remove","Permute"),object$type,sep="")
	FUN<-match.fun(tmp.fun)
	print(tmp.fun)
	if(!is.na(rand))
		set.seed(rand)
	for(i in 1:B){
		tmp.ib<-inbagg[[i]]
		oob<-which(!(1:n.obs)%in%tmp.ib)
		if(is.null(iter))
			mat.improve[,i]<-FUN(ltree[[i]],data[tmp.ib,],
				data[oob,],cl[tmp.ib],cl[oob],n.var,prob.case=prob.case)
		else
			mat.improve[,i]<-FUN(ltree[[i]],data[oob,],cl[oob],
				n.var,iter,prob.case=prob.case)
	}
	mat.improve
}

