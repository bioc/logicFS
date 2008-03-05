`getXyPred` <-
function(newData,facInfo,colOld){
	cnnew<-colnames(newData)
	formula<-facInfo$formula
	resp<-as.character(formula[[2]])
	formula<-delete.response(formula)
	cnold<-facInfo$cn
	idsY<-which(cnold==resp)
	cnold<-cnold[-idsY]
	recdom<-facInfo$recdom
	if(length(recdom)>1)
		recdom<-recdom[-idsY]
	if(length(cnold)!=length(cnnew) || any(cnold!=cnnew))
		stop("newData must contain the same variables in the same order as\n",
			"data in logic.bagging (without the response).",call.=FALSE)
	x<-getXy(formula,newData,recdom=recdom)$x
	colNew<-colnames(x)
	if(length(colNew)!=length(colOld) || any(colNew!=colOld))
		stop("It seems that some of the levels of the variables in data\n",
			"are not available in newData.",call.=FALSE)
	x
}

