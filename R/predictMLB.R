predictMLB<-function(object,newData,levs,type){
	n.lev<-length(levs)
	n.obs<-nrow(newData)
	mat.pred<-matrix(0,n.obs,n.lev)
	for(i in 1:length(object)){
		tmp.mat<-compMatProbMLR(object[[i]],newData,n.lev)
		mat.pred<-mat.pred+tmp.mat
	}
	mat.pred<-mat.pred/length(object)
	colnames(mat.pred)<-levs
	rownames(mat.pred)<-if(is.null(rownames(newData))) 1:n.obs else rownames(newData)
	if(type=="prob")
		return(mat.pred)
	ids<-max.col(mat.pred)
	factor(levs[ids],levels=levs)	
}


