make.snp.dummy<-function(data){
	if(!is.matrix(data))
		stop("'data' must be a matrix.")
	n.col<-ncol(data)
	if(is.null(colnames(data))){
		colnames(data)<-paste("SNP",1:n.col,sep="")
		warning("Since 'data' has no column names, generic ones are added.",
			call.=TRUE)
	}
	if(any(!data%in%c(1,2,3)))
		stop("Only values 1 (for homozygous reference), 2 (heterozygous)",
			"\n", "and 3 (homozygous variant) are allowed.")
	mat<-matrix(0,nrow(data),2*n.col)
	for(i in 1:n.col){
		mat[data[,i]%in%c(2,3),2*i-1]<-1
		mat[data[,i]==3,2*i]<-1
	}
	colnames(mat)<-paste(rep(colnames(data),e=2),rep(1:2,n.col),sep="_")
	mat.info<-data.frame(SNP=1:3,SNP_1=c(0,1,1),SNP_2=c(0,0,1),
		"Assumed Genotype"=c("Homozygous Reference","Heterozygous",
		"Homozygous Variant"),check.names=FALSE)
	cat("SNPs are coded as follows:\n")
	print(mat.info)
	mat
}	