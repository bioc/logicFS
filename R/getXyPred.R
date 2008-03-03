`getXyPred` <-
function(newData,facInfo,colOld){
	cnnew<-colnames(newData)
	cnold<-facInfo$cn
	if(length(cnold)!=length(cnnew) || any(cnold!=cnnew))
		stop("newData must contain the same variables in the same order as\n",
			"data in logic.bagging (without the response).",call.=FALSE)
	formula<-facInfo$formula
	tmpData<-data.frame(0,newData)
	colnames(tmpData)[1]<-as.character(formula[[2]])
	x<-getXy(formula,tmpData,recdom=facInfo$recdom)$x
	colNew<-colnames(x)
	if(length(colNew)!=length(colOld) || any(colNew!=colOld))
		stop("It seems that some of the levels of the variables in data\n",
			"are not available in newData.",call.=FALSE)
	x
}

