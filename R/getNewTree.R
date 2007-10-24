`getNewTree` <-
function(tree,col,n.var){
	mat<-tree$trees
	ids<-which(mat$knot%in%col)
	if(length(ids)>0){
		if(ids[1]==1)
			return(NULL)
		mat[ids,4]<-0
		mat[ids,3]<-ifelse(mat[ids%/%2,2]==1,n.var+1,n.var+2)
		tree$trees<-mat
	}
	tree
}

