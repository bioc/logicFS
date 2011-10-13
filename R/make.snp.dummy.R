make.snp.dummy<-function(data){
	if(!is.matrix(data))
		stop("'data' must be a matrix.")
	n.col<-ncol(data)
	if(is.null(colnames(data))){
		colnames(data)<-paste("SNP",1:n.col,sep="")
		warning("Since 'data' has no column names, generic ones are added.",
			call.=TRUE)
	}
	if(any(is.na(data)))
		warning("data contains missing values, which need to be imputed before\n",
			"using logicFS or logic.bagging.", call.=FALSE)
	r <- range(data, na.rm=TRUE)
	if(r[2]-r[1]==1)
		stop("All SNPs show only two different values.")
	if(any(r!=c(0,2)) & any(r!=c(1,3)))
		stop("All SNPs must be either coded by\n",
			"1 (for homozygous reference), 2 (heterozygous), and 3 (homozygous variant)\n",
			"or by\n",
			"0 (for homozygous reference), 1 (heterozygous), and 2 (homozygous variant).")
	mat<-matrix(0,nrow(data),2*n.col)
	mat[,seq.int(1,2*n.col,2)] <- 1*(data>r[1])
	mat[,seq.int(2,2*n.col,2)] <- 1*(data==r[2]) 
	colnames(mat)<-paste(rep(colnames(data),e=2),rep(1:2,n.col),sep="_")
	mat.info<-data.frame(SNP=r[1]:r[2],SNP_1=c(0,1,1),SNP_2=c(0,0,1),
		"Assumed Genotype"=c("Homozygous Reference","Heterozygous",
		"Homozygous Variant"),check.names=FALSE)
	cat("SNPs are coded as follows:\n")
	print(mat.info)
	mat
}	
