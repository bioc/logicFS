predict.logicBagg<-function(object,newData,prob.case=.5,...){
	if(prob.case<=0 | prob.case>=1)
		stop("prob.case must be between 0 and 1.")
	if(missing(newData))
		newData<-object$data
	trees<-object$logreg.model
	n.new<-nrow(newData)
	B<-length(trees)
	mat.pred<-matrix(NA,n.new,B)
	for(i in 1:B)
		mat.pred[,i]<-predict(trees[[i]],newbin=newData,type=object$type)
	if(object$type==3)
		mat.pred<-mat.pred>prob.case
	votes<-rowSums(mat.pred)
	pred<-rep(0,n.new)
	pred[votes>B/2]<-1
	if(any(votes==B/2))
		pred[votes==B/2]<-sample(0:1,sum(votes==B/2),replace=TRUE)
	pred
}

