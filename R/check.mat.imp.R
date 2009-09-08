check.mat.imp<-function(object,mu=0){
	if(!is(object,"logicFS") & !is(object,"logicBagg"))
		stop("object must be an object of class logicFS or logicBagg.",call.=FALSE)
	if(is(object,"logicBagg")){
		object<-object$vim
		if(is.null(object))
			stop("No information on the importances are available in object.",
				call.=FALSE)
	}
	if(object$type==2)
		warning("In the linear regression case, no standarization is necessary.",
			call.=FALSE)
	mat.imp<-object$mat.imp
	if(is.null(mat.imp)) 
		stop("No information on the improvements are available in object.\n",
			"Please use vim.logicFS with addMatImp = TRUE to generate this information,\n",
			"and then use the resulting object in vim.norm.",call.=FALSE)
	if(mu<0)
		stop("mu must be non-negative.",call.=FALSE)
	if(mu==0)
		warning("mu should actually be set to a value (slightly) larger than zero.",
			call.=FALSE)
	object$prop<-NULL
	object$mat.imp<-NULL
	return(list(mat.imp=mat.imp-mu, vim=object))
}

