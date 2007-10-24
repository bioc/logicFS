`correctPreds1` <-
function(tree,newdata,newcl,n.var,iter=NULL){
	treePreds<-eval.logreg(tree,newdata)
	if(any(treePreds>1))
		stop("Some treePreds > 1. Please inform the author about this error.")
	cortree<-sum(treePreds==newcl)
	knot<-tree$trees$knot
	knot<-knot[knot!=0]
	knot<-unique(knot)
	if(is.null(iter))
		return(correctPredsRemove1(tree,newdata,newcl,n.var,knot,cortree))
	correctPredsPermute1(tree,newdata,newcl,n.var,iter,knot,cortree)
}

