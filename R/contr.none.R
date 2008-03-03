`contr.none` <-
function(n,contrasts=FALSE){
	tmp<-ifelse(length(n)==2,TRUE,FALSE)
	contr.treatment(n,contrasts=tmp)
}

