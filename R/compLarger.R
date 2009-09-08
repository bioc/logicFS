compLarger<-function(mat.imp,mat.perm,stat,B,type){
	x.bar<-mat.imp%*%mat.perm
	if(type==2)
		mat.stat <- x.bar/B
	else{
		x.bar<-x.bar/B
		x2<-rowMeans(mat.imp*mat.imp)
		sd<-x2-x.bar^2
		sd<-sqrt(sd)
		n<-sqrt(B-1)
		mat.stat<-n*x.bar/sd
	}
	rowSums(stat<=mat.stat)
}

