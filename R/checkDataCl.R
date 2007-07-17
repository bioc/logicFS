`checkDataCl` <-
function(object,data=NULL,cl=NULL){
	if(!is(object,"logicBagg") & !is(object,"logicFS"))
		stop("object must be an object of either class logicBagg or class logicFS.",call.=FALSE)
	if(!is.null(data) & is.null(cl))
		stop("If data is specified, cl must also be specified.",call.=FALSE)
	if(is.null(data) & !is.null(cl))
		stop("data must also be specified, if cl is specified.",call.=FALSE)
	if(!is.null(data) & !is.null(cl)){
		if(!is.data.frame(data) & !is.matrix(data))
			stop("data must be a data frame or a matrix.",call.=FALSE)
		if(length(cl)!=nrow(data))
			stop("The length of cl must be equal to the number of rows of data.",call.=FALSE)
		if(any(is.na(cl)))
			stop("No missing values allowed.",call.=FALSE)
		if(any(is.na(data)))
			stop("No missing values allowed.",call.=FALSE)
		if(any(!cl%in%c(0,1)))
			stop("cl must consist of 0's and 1's.",call.=FALSE)
		if(any(!as.matrix(data)%in%c(0,1)))
			stop("data must consist of 0's and 1's.",call.=FALSE)
	}
	if(is(object,"logicBagg")){
		if(is.null(data) & is.null(cl)){
			data<-object$data
			cl<-object$cl
		}
		object<-object$vim
		if(is.null(object))
			stop("Specification of the interactions is not available in object.",call.=FALSE)
	}
	if(is(object,"logicFS")){
		if(is.null(data) | is.null(cl))
			stop("data and cl must be specified if object is of class logicFS.",call.=FALSE)
	}
	if(is.matrix(data))
		data<-as.data.frame(data)
	return(list(data=data,cl=cl,vim=object))
}

