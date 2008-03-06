`predict.mlogreg` <-
function(object,newData,type=c("class","prob"),...){
	if(missing(newData))
		newData<-object$data
	else{
		if(any(is.na(newData)))
			stop("No missing values allowed.")
		colOld<-colnames(object$data)
		if(!is.null(object$facInfo))
			newData<-getXyPred(newData,object$facInfo,colOld)
		else{
			colNew<-colnames(newData)
			if(length(colOld)!=length(colNew) || any(colOld!=colNew))
				stop("newData must contain the same variables in the same order as\n",
					"the data matrix in mlogreg (without the response if the\n",
					"formula method has been used).")
		}
		newData<-as.matrix(newData)
		if(any(!newData %in% c(0,1)))
			stop("newData must only contain binary variables with values 0 and 1.")
	}
	models<-object$logreg.model
	levs<-levels(object$cl)
	n.lev<-length(levs)
	mat.prob<-matrix(0,nrow(newData),n.lev)
	for(i in 2:n.lev)
		mat.prob[,i]<-predict(models[[i-1]],newData,2)
	mat.prob<-exp(mat.prob)
	mat.prob<-mat.prob/rowSums(mat.prob)
	colnames(mat.prob)<-levs
	rownames(mat.prob)<-if(is.null(rownames(newData))) 1:nrow(newData) else rownames(newData)
	type<-match.arg(type)
	if(type=="prob")
		return(mat.prob)
	ids<-max.col(mat.prob)
	factor(levs[ids],levels=levs)
}

