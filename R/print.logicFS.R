print.logicFS<-function(x,topX=5,show.prop=TRUE,coded=FALSE,digits=2,...){
	param<-x$param
	if(!is.null(param)){
		cat("Selection of Interactions Using Logic Regression\n\n")
		cat("Number of Iterations: ",param$B,"\n")
		cat("Sampling Method:",param$sampling,"\n")
		cat("Logic Regression Type:",ifelse(x$type==1,"Classification",
			"Logistic Regression"),"\n")
		if(x$type==3)
			cat("Number of Trees:      ",param$ntrees,"\n")
		cat("Max. Number of Leaves:",param$nleaves,"\n\n")
	}
	cat("Importance Measure:",gsub("\n","",x$measure),"\n")
	if(!is.null(x$mu))
		cat("(Alternative Hypothesis: Importance is greater than ",x$mu,")\n",sep="")
	cat("\n")
	if(!is.null(param))
		cat("\n")
	vim<-sort(x$vim,decreasing=TRUE)
	topX<-min(topX,length(vim))
	names.vim<-if(coded) names(vim) else x$primes[order(x$vim,decreasing=TRUE,na.last=NA)]
	out<-data.frame(Importance = vim,
		Proportion = if(!is.null(x$prop)) x$prop[order(x$vim,decreasing=TRUE,na.last=NA)] else NA,
		Expression = names.vim)
	rownames(out)<-1:nrow(out)
	if(!show.prop | is.null(x$prop))
		out<-out[,-2]
	out<-format(out[vim>=vim[topX],],digits=digits,nsmall=2)
	cat("The",nrow(out),"Most Important Interactions:\n\n")
	print(out)
}

