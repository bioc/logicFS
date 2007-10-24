`getKnots` <-
function(tree){
	if(is(tree,"logregtree"))
		knot<-tree$trees$knot
	else
		knot<-unlist(lapply(tree,function(x) x$trees$knot))
	knot<-knot[knot!=0]
	unique(knot)
}

