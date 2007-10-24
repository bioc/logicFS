`correctPredsPermute1` <-
function(tree,newdata,newcl,n.var,iter,knot,cortree){
	corpreds<-numeric(n.var)
	le<-getTree(tree,add="X")
	mat.samp<-matrix(0,length(newcl),iter)
	newdata<-as.data.frame(newdata)
	colnames(newdata)<-paste("X",1:n.var,"X",sep="")
	attach(newdata)
	for(i in knot){
		tmp<-newdata[,i]
		for(j in 1:iter)
			mat.samp[,j]<-sample(tmp)
		new.le<-gsub(paste("X",i,"X",sep=""),"mat.samp",le)
		mat.eval<-eval(parse(text=new.le))
		corperm<-colSums(mat.eval==newcl)
		corpreds[i]<-cortree-mean(corperm)
	}
	detach(newdata)
	corpreds
}

