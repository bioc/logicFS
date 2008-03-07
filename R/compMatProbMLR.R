compMatProbMLR<-function(trees,newData,n.lev){
	mat<-matrix(0,nrow(newData),n.lev)
	for(j in 2:n.lev)
		mat[,j]<-predict(trees[[j-1]],newData,2)
	mat<-exp(mat)
	mat/rowSums(mat)
}

