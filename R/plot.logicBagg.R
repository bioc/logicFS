plot.logicBagg<-function(x,topX=15,cex=.9,pch=16,col=1,show.prop=FALSE,force.topX=FALSE,
		coded=TRUE,include0=TRUE,add.v0=TRUE,v0.col="grey50",main=NULL,...){
	if(is.null(x$vim))
		stop("No information about the importance available.")
	plot(x$vim,topX=topX,cex=cex,pch=pch,col=col,show.prop=show.prop,
		force.topX=force.topX,coded=coded,include0=include0,add.v0=add.v0,
		v0.col=v0.col,main=main,...)
}

