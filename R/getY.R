`getY` <-
function(mf){
	y<-model.response(mf)
	if(!is.factor(y))
		return(y)
	if(length(unique(y))==2)
		return(as.numeric(y)-1)
	y
}

