plot.logicFS<-function(x,topX=15,cex=.9,pch=16,col=1,v0.col="grey35",show.prop=FALSE,
		force.topX=FALSE,include0=TRUE,coded=TRUE,...){
	if(!show.prop){
		vim<-x$vim
		main<-paste(ifelse(x$type==1,"Single","Multiple"),"Tree Measure")
		xlab<-"Importance"
	}
	else{
		vim<-x$prop
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
	if(!show.prop & rangex[1]<=0)
		abline(v=0,lty="dashed",col=v0.col)
}

