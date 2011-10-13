`modelMat` <-
function(formula,mf,cn,isFac,recdom=TRUE){
	if(!is.logical(recdom))
		stop("recdom must be logical.",call.=FALSE)
	if(!length(recdom)%in%c(1,length(cn)))
		stop("recdom must be either of length 1 or ncol(data).",call.=FALSE)
	nlev<-sapply(mf, function(x) length(levels(x)))
	cnmf<-colnames(mf)
	if(length(formula)==3){
		nlev<-nlev[-1]
		cnmf<-cnmf[-1]
		isFac<-isFac[-1]
	}
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
		r <- as.numeric(range(tmp.mat))
		if(r[2]-r[1]==1)
			stop("None of the SNPs for which recdom=TRUE show more than two genotypes.")
		if(any(r!=c(0,2)) & any(r!=c(1,3)))
			stop("Levels of all SNP factors must be either coded by\n\n",
				"1 for the homozygous reference genotype,\n",
				"2 for the heterozygous genotype, and\n",
				"3 for the homozygous variant genotype,\n\n",
				"or by\n\n",
				"0 for the homozygous reference genotype,\n",
				"1 for the heterozygous genotype, and\n",
				"2 for the homozygous variant genotype.", call.=FALSE)
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

