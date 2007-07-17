plot.logicFS<-function(x,topX=15,cex=.9,pch=16,col=1,show.prop=FALSE,force.topX=FALSE,coded=TRUE,
		add.thres=TRUE,thres=NULL,include0=TRUE,add.v0=TRUE,v0.col="grey50",...){
	if(!show.prop){
		vim<-x$vim
		main<-paste(x$measure,"Measure")
		xlab<-"Importance"
	}
	else{
		vim<-x$prop
		if(is.null(vim))
			stop("Data for the Ad Hoc Measure are not available.")
		main<-"Ad Hoc Measure"
		xlab<-"Proportion"
	}
	if(!coded)
		names(vim)<-x$primes
	vim<-sort(vim,decreasing=TRUE)
	topX<-min(topX,length(vim))
	vim<-if(force.topX) vim[1:topX] else vim[vim>=vim[topX]]
	rangex<-if(!show.prop) range(if(include0) 0,vim) else c(0,1)
	dotchart(rev(vim),main=main,xlab=xlab,pch=pch,col=col,cex=cex,xlim=rangex)
	if(!show.prop & add.v0 & rangex[1]<=0)
		abline(v=0,lty="dotted",col=v0.col)
	if(add.thres){
		if(is.null(thres))
			thres<-x$threshold
		if(!is.null(thres))
			abline(v=thres,lty="dashed")
	}
}

