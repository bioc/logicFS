vim.permInput <- function(object, n.perm=NULL, standardize=TRUE, rebuild=FALSE,
		prob.case=0.5, useAll=FALSE, adjust="bonferroni", addMatPerm=FALSE, 
		rand=NA){
	if(!is(object, "logicBagg"))
		stop("object must be an object of class logicBagg.")
	type <- object$type
	if(!type %in% c(1,3))
		stop("Currently only available for classification and logistic",
			" regression.")
	whichType <- type - rebuild * (type==3)
	if(is.null(n.perm))
		n.perm <- ifelse(whichType==2, 100, 1000)
	ltree <- object$logreg.model
	cl <- object$cl
	data <- object$data
	n.var <- ncol(data)
	n.obs <- length(cl)
	oob <- lapply(object$inbagg, function(x) which(!(1:n.obs) %in% x))
	mat.cl <- matrix(0, n.obs, n.perm + 1)
	mat.cl[,1] <- cl
	if(!is.na(rand))
		set.seed(rand)
	for(i in 2:(n.perm + 1))
		mat.cl[,i] <- sample(cl)
	mat.in <- getVarInTree(ltree, n.var, type=type)
	mat.Nb <- compNb(ltree, data, oob, mat.cl, type, prob.case=prob.case)
	mat.perm <- matrix(0, n.var, n.perm + 1)
	for(i in 1:n.var){
		ids <- which(mat.in[i,]==1)
		if(length(ids) > 0)
			mat.perm[i,] <- switch(whichType, 
				compPermInput1(ltree[ids], i, mat.Nb[,ids, drop=FALSE],
					mat.cl, data, oob[ids], n.var, length(ltree),
					standardize=standardize),
				compPermInput3Rebuild(ltree[ids], i, mat.Nb[,ids, drop=FALSE],
					mat.cl, data, oob[ids], n.var, length(ltree),
					object$inbagg[ids], standardize=standardize,
					prob.case=prob.case),
				compPermInput3Fast(ltree[ids], i, mat.Nb[,ids, drop=FALSE],
					mat.cl, data, oob[ids], n.var, length(ltree),
					standardize=standardize, prob.case=prob.case))
	} 
	if(!useAll)
		pval <- rowMeans(mat.perm[,1] <= mat.perm[,-1], na.rm=TRUE)
	else{
		pval <- numeric(n.var)
		for(i in 1:n.var)
			pval[i] <- mean(mat.perm[i,1] <= mat.perm[,-1], na.rm=TRUE)
	}
	pval <- adjustPval(pval, adjust=adjust)
	vim <- 1-pval
	names(vim) <- colnames(data)
	measure <- if(adjust=="none") "Unadjusted" 
		else paste(toupper(adjust), "Adjusted\n")
	measure <- paste(if(standardize) "Standardized", measure, "Permutation Based",
		"Input")
	if(!addMatPerm)
		mat.perm <- NULL
	out <- list(vim=vim, prop=NULL, primes=names(vim), type=type, param=NULL, 
		mat.imp=NULL, measure=measure, threshold=0.95, mu=NULL, useN=TRUE,
		name="Variable", mat.perm=mat.perm)
	class(out) <- "logicFS"
	out
}
	 


compNb <- function(ltree, data, oob, mat.cl, type, prob.case=0.5){
	mat <- matrix(0, ncol(mat.cl), length(ltree))
	for(i in 1:length(ltree)){
		treePreds <- as.vector(predict(ltree[[i]], data[oob[[i]],], type))
		if(type==3)
			treePreds <- treePreds > prob.case
		mat[,i] <- colSums(treePreds == mat.cl[oob[[i]], ])
	}
	mat
}



compPermInput1 <- function(listTrees, whichVar, Nb, mat.cl, data, listOOB, n.var, B, 
		standardize=TRUE){
	for(i in 1:length(listTrees)){
		tmpDat <- cbind(data[listOOB[[i]],], 1, 0)
		newTree <- getNewTree(listTrees[[i]]$trees[[1]], whichVar, n.var)
		preds <- as.vector(eval.logreg(newTree, tmpDat))
		if(any(preds > 1))
			stop("Some preds > 1. Please inform the author about this error.")
		Nb[,i] <- Nb[,i] - colSums(preds == mat.cl[listOOB[[i]],])
	}
	out <- rowSums(Nb) / B
	if(standardize){
		tmp <- rowSums((Nb-out)^2) + (B-length(listTrees)) * out^2
		out <- sqrt(B*(B-1)) * out / sqrt(tmp)
	}
	out		
}


compPermInput3Fast <- function(listTrees, whichVar, Nb, mat.cl, data, listOOB, n.var, B,
		standardize=TRUE, prob.case=0.5){
	for(i in 1:length(listTrees)){
		tmpDat <- cbind(data[listOOB[[i]],], 1, 0)
		newtree <- lapply(listTrees[[i]]$trees, getNewTree, whichVar, n.var)
		idsIn <- !unlist(lapply(newtree, is.null))
		listTrees[[i]]$trees <- newtree[idsIn]
		preds <- as.vector(predict(listTrees[[i]], tmpDat, 3))
		if(any(preds>1 | preds<0))
			stop("Something went wrong. Please inform the author.")
		preds <- preds > prob.case
		Nb[,i] <- Nb[,i] - colSums(preds == mat.cl[listOOB[[i]],])
	}
	out <- rowSums(Nb) / B
	if(standardize){
		tmp <- rowSums((Nb-out)^2) + (B-length(listTrees)) * out^2
		out <- sqrt(B*(B-1)) * out / sqrt(tmp)
	}
	out
}


compPermInput3Rebuild <- function(listTrees, whichVar, Nb, mat.cl, data, listOOB, n.var,
		B, listInbagg, standardize=TRUE, prob.case=0.5){
	for(i in 1:length(listTrees)){
		newdata <- cbind(data[listOOB[[i]],], 1, 0)
		olddata <- cbind(data[listInbagg[[i]],], 1, 0)
		newtree <- lapply(listTrees[[i]]$trees, getNewTree, whichVar, n.var)
		idsIn <- !unlist(lapply(newtree, is.null))
		newtree <- newtree[idsIn]
		mat.design <- cbind(1, sapply(newtree, eval.logreg, olddata))
		mat.new <- cbind(1, sapply(newtree, eval.logreg, newdata))
		for(j in 1:ncol(mat.cl)){
			coef <- glm.fit(mat.design, mat.cl[listInbagg[[i]], j],
				family=binomial())$coefficients
			preds <- as.vector(mat.new %*% coef)
			preds <- exp(preds) / (1+exp(preds))
			if(any(preds>1 | preds<0))
				stop("Something went wrong. Please inform the author.")
			preds <- preds > prob.case
			Nb[j,i] <- Nb[j,i] - sum(preds == mat.cl[listOOB[[i]],j])
		}
	}
	out <- rowSums(Nb) / B
	if(standardize){
		tmp <- rowSums((Nb-out)^2) + (B-length(listTrees)) * out^2
		out <- sqrt(B*(B-1)) * out / sqrt(tmp)
	}
	out
}


