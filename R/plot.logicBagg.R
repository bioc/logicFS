plot.logicBagg<-function(x,topX=15,cex=.9,pch=16,col=1,v0.col="grey35",show.prop=FALSE,
		force.topX=FALSE,include0=TRUE,coded=TRUE,...){
	if(is.null(x$vim))
		stop("No information about the importance available.")
	plot(x$vim,topX=topX,cex=cex,pch=pch,col=col,v0.col=v0.col,show.prop=show.prop,
		force.topX=force.topX,include0=include0,coded=coded)
}

