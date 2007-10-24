`getTree` <-
function(tree,add=""){
	if(is(tree,"logregmodel"))
		tree<-tree$trees[[1]]
	if(is(tree,"logregtree"))
		tree<-tree$trees
	lexp<-paste(ifelse(tree$neg,"!",""),"X",tree$knot,add,sep="")
	for(i in 1:length(lexp))
		lexp[i]<-switch(tree$conc[i] + 1, "", " & ", " | ", lexp[i])
	for(i in length(lexp):1){
		if(tree$conc[i]%in%(1:2))
			lexp[i]<-paste("(", lexp[2*i], lexp[i], lexp[2*i+1], ")",
				sep="")
	}
	lexp[1]
}

