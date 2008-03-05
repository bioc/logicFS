`getXy` <-
function(formula,data,recdom=TRUE,onlyBin=TRUE){	
	if(!is.data.frame(data))
		stop("data must be a data frame.")
	if(any(is.na(data)))
		stop("No missing values allowed.")
	mf<-model.frame(formula,data=data)
	isFac<-sapply(mf,is.factor)
	if(onlyBin){
		tmp<-as.matrix(mf[,!isFac])
		if(any(!tmp%in%c(0,1)))
			stop("Variables must be either factors or binary with values 0 and 1.",
				call.=FALSE)
	}
	y<-getY(mf)
	if(sum(isFac[-1])==0){
		x<-model.matrix(formula,mf)[,-1,drop=FALSE]
		facInfo<-NULL
	}
	else{
		cn<-colnames(data)
		x<-modelMat(formula,mf,cn,isFac,recdom=recdom)
		facInfo<-list(formula=attr(mf,"terms"),cn=cn,recdom=recdom)
	}
	return(list(x=x,y=y,facInfo=facInfo))
}

