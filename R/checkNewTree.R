`checkNewTree` <-
function(tree,n.var){
	knots<-getKnots(tree)
	if(is.null(knots) | all(knots>n.var))
		return(NULL)
	mat<-tree$trees
	addvar<-n.var+(1:2)
	for(i in seq(nrow(mat),3,-2)){
		if(all(mat[c(i,i-1),3]%in%addvar)){
			mat[c(i,i-1),2:4]<-0
			newids<-i%/%2
			mat[newids,2]<-3
			mat[newids,3]<-ifelse(mat[newids%/%2,2]==1,addvar[1],addvar[2])
		}
	}
	tree$trees<-mat
	tree
}

