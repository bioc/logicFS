`compMatImpIndividual1` <-
function(object,useN=FALSE,iter=10,rand=NA){
	inbagg<-object$inbagg
	ltree<-object$logreg.model
	data<-object$data
	cl<-object$cl
	n.var<-ncol(data)
	n.obs<-nrow(data)
	B<-length(ltree)
	mat.improve<-matrix(0,n.var,B)
	if(!is.na(rand))
		set.seed(rand)
	for(i in 1:B){
		oob<-which(!(1:n.obs)%in%inbagg[[i]])
		tree<-ltree[[i]]$trees[[1]]
		mat.improve[,i]<-correctPreds1(tree,data[oob,],cl[oob],n.var,
			iter=iter)
		if(!useN)
			mat.improve[,i]<-mat.improve[,i]/length(oob)
	}
	mat.improve
}

