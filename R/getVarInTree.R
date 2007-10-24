`getVarInTree` <-
function(ltree,n.var){
	B<-length(ltree)
	mat<-matrix(0,n.var,B)
	for(i in 1:B){
		tmp<-getKnots(ltree[[i]]$trees)
		mat[tmp,i]<-1
	}
	mat
}

