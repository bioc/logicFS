`checkSet` <-
function(set,m,cn){
	if(is.null(set)){
		if(is.null(cn))
			stop("Column names of data must be specified if set=NULL.",call.=FALSE)
		set<-substring(cn,1,nchar(cn)-2)
	}
	if(!is.vector(set))
		stop("set must either be a vector or a list.",call.=FALSE)
	if(is.list(set)){
		tmp<-unlist(set)
		if(!is.integer(tmp) & !is.character(tmp))
			stop("The values in set must be either of type integer or character.",call.=FALSE)
		tmp<-unique(tmp)
		if(is.numeric(tmp) && any(!tmp%in%(1:m)))
			stop("Some of the values in set do not specify a column of data.",call.=FALSE)
		if(is.character(tmp)){
			if(any(!tmp%in%cn))
				stop("Some of the names in set do not specify a column of data.",call.=FALSE)
			set<-lapply(set,function(x) which(cn%in%x))
		}
		if(is.null(names(set)))
			names(set)<-paste("Set",1:length(set),sep="")
		return(set)
	}
	if(length(set)!=m)
		stop("The length of set must be equal to the number of columns of data.",call.=FALSE)
	set2<-na.exclude(set)
	n.set<-length(unique(na.omit(set2)))
	if(length(set2)==n.set)
		stop("None of the sets contains more than one variable. Please use\n", 
			"vim.individual if you would like to quantify the importance of\n",
			"single variables.",call.=FALSE)
	if(is.numeric(set)){
		if(any(!set2%in%(1:n.set)))
			stop("set must consist of the integers between 1 and ", n.set,".",call.=FALSE)
		set<-split((1:m),set)
		names(set)<-paste("Set",1:n.set,sep="")
		return(set)
	}
	if(is.character(set))
		return(split((1:m),set))
	stop("set is neither numeric nor a character vector.",call.=FALSE)
}

