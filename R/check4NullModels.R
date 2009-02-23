check4NullModels <- function(lp){
	allNull <- function(x) all(sapply(x, is.null))
	whichNull <- which(sapply(lp, allNull))
	if(length(whichNull)>0){
		lp <- lp[-whichNull]
		warning("Since ", length(whichNull), " of the models contain no variables, ",
			"they are removed.", call.=FALSE)
	}
	lp
}

