logic.oob<-function(log.out,prob.case=0.5){
	if(!is(log.out,"logicBagg"))
		stop("log.out must be an object of class logicBagg.")
	data<-log.out$data
	trees<-log.out$logreg.model
	inbagg<-log.out$inbagg
	type<-log.out$type
	n.row<-nrow(data)
	votes<-n.in<-pred<-numeric(n.row)
	for(i in 1:length(trees)){
		oob<-which(!(1:n.row)%in%inbagg[[i]])
		pred.cl<-predict(trees[[i]],data[oob,],type)
		if(type%in%c(1,3))
			pred.cl<-pred.cl>prob.case
		votes[oob]<-votes[oob]+pred.cl
		n.in[oob]<-n.in[oob]+1
	}
	if(any(n.in==0)){
		warning(sum(n.in==0)," of the observations are in none of the oob samples.")
		pred[n.in==0]<-NA
	}
	if(type==2){
		pred<-votes/n.in
		oob.err<-sqrt(mean((log.out$cl-pred)^2, na.rm=TRUE))
	}
	else{	
		pred[votes>n.in/2]<-1
		if(any(votes==n.in/2))
			pred[votes==n.in/2]<-sample(0:1,sum(votes==n.in/2),rep=TRUE)
		oob.err<-mean(pred!=log.out$cl,na.rm=TRUE)
	}
	oob.err
}

