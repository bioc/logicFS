`modelMat` <-
function(formula,mf,cn,isFac,recdom=TRUE){
	if(!is.logical(recdom))
		stop("recdom must be logical.",call.=FALSE)
	if(!length(recdom)%in%c(1,length(cn)))
		stop("recdom must be either of length 1 or ncol(data).",call.=FALSE)
	nlev<-sapply(mf[-1], function(x) length(levels(x)))
	cnmf<-colnames(mf)[-1]
	isFac<-isFac[-1]
	if(any(nlev>9))
		stop("Maximum number of levels a variable is allowed to have is 9.",call.=FALSE)
	if(length(recdom)==1)
		recdom<- recdom & isFac & (nlev==3)
	else{
		ids<-match(cnmf,cn)
		recdom<-recdom[ids]
		if(any(recdom & (nlev>3)))
			stop("recdom is set to TRUE for some variables with more than three levels.",
				call.=FALSE)
		if(any(recdom & !isFac))
			stop("For numeric variables, recdom has to be set to FALSE.",call.=FALSE)
	}
	if(any(recdom)){
		tmp.mat<-as.matrix(mf[,cnmf[recdom]])
		if(any(!tmp.mat%in%1:3))
			stop("Levels of SNP factors must be coded by 1, 2, and 3, where\n",
				"1 codes for the homozygous reference genotype,\n",
				"2 for the heterozygous genotype, and\n",
				"3 for the homozygous variant genotype.",call.=FALSE)
	}	
	contrasts<-makeContrastList(cnmf[isFac],recdom[isFac])
	x<-model.matrix(formula,mf,contrasts.arg=contrasts)
	cols<-attr(x,"assign")
	idscn<-which(cols%in%which(isFac))
	cn<-colnames(x)[idscn]
	ncn<-nchar(cn)
	colnames(x)[idscn]<-paste(substring(cn,1,ncn-1),substring(cn,ncn,ncn),sep="_")
	x[,-1,drop=FALSE]
}

