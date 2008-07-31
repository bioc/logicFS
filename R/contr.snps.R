`contr.snps` <-
function(n,contrasts=TRUE){
	if(length(n)==2){
		warning("One of the SNPs shows only 2 levels, and is therefore not transformed\n",
			"into two variables coding for a dominant and a recessive effect.",
			call.=FALSE)
		return(contr.treatment(n))
	}
	tmp<-contr.treatment(n,contrasts=TRUE) 
	tmp[3,1]<-1 
	colnames(tmp)<-c("D","R")
	tmp
}

