`makeContrastList` <-
function(cn,recdom){
	contrast<-ifelse(recdom,"contr.snps","contr.none")
	tmp<-as.list(contrast)
	names(tmp)<-cn
	tmp
}

