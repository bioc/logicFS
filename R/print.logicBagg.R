print.logicBagg<-function(x,...){
	cat("Bagged Logic Regression\n\n")
	cat("Number of Iterations: ",length(x$logreg.model),"\n")
	cat("Sampling Method:      ",x$sampling,"\n")
	cat("Logic Regression Type:",switch(x$type,"Classification","Linear Regression",
		"Logistic Regression"),"\n")
	if(x$type%in%c(2,3))
		cat("Number of Trees:      ",x$ntrees,"\n")
	cat("Max. Number of Leaves:",x$nleaves,"\n")
	if(!is.null(x$oob.error)){
		if(x$type==2)
			cat("\n","OOB RMSPE:             ",round(x$oob.error,3),"\n",sep="")
		else
	   		cat("\n","OOB Error Rate:        ",100*round(x$oob.error,4),"%","\n",sep="")
	}
}

