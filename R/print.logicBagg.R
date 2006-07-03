print.logicBagg<-function(x,...){
	cat("Bagged Logic Regression\n\n")
	cat("Number of Iterations: ",length(x$logreg.model),"\n")
	cat("Logic Regression Type:",ifelse(x$type==1,"Classification",
		"Logistic Regression"),"\n")
	if(x$type==3)
		cat("Number of Trees:      ",x$ntrees,"\n")
	cat("Max. Number of Leaves:",x$nleaves,"\n")
	if(!is.null(x$oob.error))
	   cat("\n","OOB Error Rate:        ",100*round(x$oob.error,4),"%","\n",sep="")
}

