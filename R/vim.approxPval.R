vim.approxPval <- function(object, version=1, adjust="bonferroni"){
	if(!is(object, "logicFS"))
		stop("object must be an object of class logicFS.")
	if(!version %in% (1:2))
		stop("version must be either 1 or 2.")
	stand <- grepl("Standardized", object$measure)
	type <- object$type
	if(type!=2 && !stand)
		stop("The importances must be standardized -- ",
			"except for the linear regression case.")
	B <- findB(object, stand=stand)
	if(stand)
		pval <- pt(object$vim, B-1, lower.tail=FALSE)
	else
		pval <- pf(object$vim, B, B, lower.tail=FALSE)
	pval <- adjustPval(pval, adjust=adjust)
	vim <- if(version==1) 1-pval else -log10(pval)
	names(vim) <- names(object$vim)
	object$vim <- vim
	object$measure <- "Approximate P-Value Based"
	object$threshold <- if(version==1) 0.05 else -log10(0.05)
	object
} 
	


findB <- function(object, stand=TRUE){
	if(!is.null(object$param$B))
		return(object$param$B)
	if(!is.null(object$mat.imp))
		return(ncol(object$mat.imp))
	if(is.null(object$threshold))
		stop("Cannot find any information on how many iterations have been",
			" used in logicFS.\n", "Please recompute object, and set ",
			"addMatImp=TRUE in this computation.")
	
	if(stand)
		vecq <- qt(1-0.05/length(object$vim), 1:500)
	else
		vecq <- qf(1-0.05/length(object$vim), 1:500, 1:500)	
	B <- which(vecq==object$threshold)
	if(length(B)!=1)
		stop("Cannot determine how many iterations have been used in logicFS.\n",
			"Please recompute object, and set addMatImp=TRUE in this computation.")
	if(stand)
		B <- B+1
	warning("Since no direct information about the number of iterations used in logicFS",
		"\n", "is available in object, this number has been number has been determined",
		"\n", "and set to B=", B, ".", call.=FALSE)
	B
}
