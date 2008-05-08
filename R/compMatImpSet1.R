`compMatImpSet1` <-
function(object,set,iter=NULL,rand=NA){
	inbagg<-object$inbagg
	ltree<-object$logreg.model
	data<-object$data
	cl<-object$cl
	n.var<-ncol(data)
	n.obs<-nrow(data)
	n.set<-length(set)
	B<-length(ltree)
	mat.improve<-matrix(0,n.set,B)
	if(!is.null(iter) && !is.na(rand))
		set.seed(rand)
	for(i in 1:B){
		oob<-which(!(1:n.obs)%in%inbagg[[i]])
		tree<-ltree[[i]]$trees[[1]]
		if(is.null(iter))
			mat.improve[,i]<-correctSetRemove1(tree,data[oob,],cl[oob],set,n.var,n.set)
		else
			mat.improve[,i]<-correctSetPermute1(tree,data[oob,],cl[oob],set,n.var,n.set,iter)
		if(!useN)
			mat.improve[,i]<-mat.improve[,i]/length(oob)
	}
	mat.improve
}

