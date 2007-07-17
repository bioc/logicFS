predict.logicBagg<-function(object,newData,prob.case=.5,type=c("class","prob"),...){
	if(prob.case<=0 | prob.case>=1)
		stop("prob.case must be between 0 and 1.")
	if(missing(newData))
		newData<-object$data
	else{
		newData<-as.matrix(newData)
		if(any(is.na(newData)))
			stop("No missing values allowed.")
		if(any(!newData%in%c(0,1)))
			stop("newData must only contain binary variables coded by 0 and 1.")
		colOld<-colnames(object$data)
		colNew<-colnames(newData)
		if(length(colOld)!=length(colNew) || any(colOld!=colNew))
			stop("newData must contain the same variables in the same order as\n",
				"the data matrix in logic.bagging (without the response if the formula\n",
				"method has been used).")
	}
	trees<-object$logreg.model
	n.new<-nrow(newData)
	B<-length(trees)
	mat.pred<-matrix(NA,n.new,B)
	type<-match.arg(type)
	for(i in 1:B)
		mat.pred[,i]<-predict(trees[[i]],newData,object$type)
	pred<-rowMeans(mat.pred)
	if(type=="prob")
		return(pred)
	pred<-as.numeric(pred>prob.case)	
	if(any(pred==prob.case))
		pred[pred==prob.case]<-sample(0:1,sum(pred==prob.case),replace=TRUE)
	pred
}

