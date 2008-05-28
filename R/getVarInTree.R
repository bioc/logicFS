`getVarInTree` <-
function(ltree,n.var,type=0){
	if(type==9){
		tmp.fun<-function(x,n.var) rowSums(getVarInTree(x,n.var))>0
		mat<-sapply(ltree,tmp.fun,n.var=n.var)
		return(mat)
	}
	B<-length(ltree)
	mat<-matrix(0,n.var,B)
	for(i in 1:B){
		tmp<-getKnots(ltree[[i]]$trees)
		mat[tmp,i]<-1
	}
	mat
}

