vim.permSNP <- function(object, n.perm=NULL, standardize=TRUE, rebuild=FALSE,
		prob.case=0.5, useAll=FALSE, version=1, adjust="bonferroni", 
		addMatPerm=FALSE, rand=NA){
	out <- vim.permSet(object, n.perm=n.perm, standardize=standardize,
		rebuild=rebuild, prob.case=prob.case, useAll=useAll, version=version,
		adjust=adjust, addMatPerm=addMatPerm, rand=rand)
	out$measure <- gsub("Set", "SNP", out$measure)
	out$name <- "SNP"
	out
}


vim.permSet <- function(object, set=NULL, n.perm=NULL, standardize=TRUE, rebuild=FALSE,
		prob.case=0.5, useAll=FALSE, version=1, adjust="bonferroni", 
		addMatPerm=FALSE, rand=NA){
	if(!is(object, "logicBagg"))
		stop("object must be an object of class logicBagg.")
	if(!version %in% (1:2))
		stop("version must be either 1 or 2.")
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
	set <- checkSet(set, n.var, colnames(data))
	oob <- lapply(object$inbagg, function(x) which(!(1:n.obs) %in% x))
	mat.cl <- matrix(0, n.obs, n.perm + 1)
	mat.cl[,1] <- cl
	if(!is.na(rand))	
		set.seed(rand)
	for(i in 2:(n.perm + 1))
		mat.cl[,i] <- sample(cl)
	mat.in <- getVarInTree(ltree, n.var, type=type)
	mat.Nb <- compNb(ltree, data, oob, mat.cl, type, prob.case=prob.case)
	n.set <- length(set)
	mat.perm <- matrix(0, n.set, n.perm + 1)
	for(i in 1:n.set){
		ids <- which(colSums(mat.in[set[[i]],]) > 0)
		if(length(ids) > 0)
			mat.perm[i,] <- switch(whichType,
				compPermSet1(ltree[ids], set[[i]], mat.Nb[,ids, drop=FALSE],
					mat.cl, data, oob[ids], n.var, length(ltree),
					standardize=standardize),
				compPermSet3Rebuild(ltree[ids], set[[i]], mat.Nb[,ids, drop=FALSE],
					mat.cl, data, oob[ids], n.var, length(ltree),
					object$inbagg[ids], standardize=standardize,
					prob.case=prob.case),
				compPermSet3Fast(ltree[ids], set[[i]], mat.Nb[,ids, drop=FALSE],
					mat.cl, data, oob[ids], n.var, length(ltree),
					standardize=standardize, prob.case=prob.case))
	}
	if(!useAll)
		pval <- rowMeans(mat.perm[,1] <= mat.perm[,-1], na.rm=TRUE)
	else{
		pval <- numeric(n.set)
		for(i in 1:n.set)
			pval[i] <- mean(mat.perm[i,1] <= mat.perm[,-1], na.rm=TRUE)
	}
	if(version==2)
		pval[pval==0] <- (10*n.perm)^-1
	pval <- adjustPval(pval, adjust=adjust)
	vim <- if(version==1) 1-pval else -log10(pval)
	#pval <- adjustPval(pval, adjust=adjust)
	#vim <- 1-pval	
	names(vim) <- names(set)
	measure <- if(adjust=="none") "Unadjusted"
		else paste(toupper(adjust), "Adjusted\n")
	measure <- paste(if(standardize) "Standardized", measure, "Permutation Based",
		"Set")
	if(!addMatPerm)
		mat.perm <- NULL
	thres <- ifelse(version==1, 0.95, -log10(0.05))
	out <- list(vim=vim, prop=NULL, primes=names(set), type=type, param=NULL,
		mat.imp=NULL, measure=measure, threshold=thres, mu=NULL, useN=TRUE,
		name="Set", mat.perm=mat.perm)
	class(out) <- "logicFS"
	out
}
	


compPermSet1 <- function(listTrees, set, Nb, mat.cl, data, listOOB, n.var, B,
		standardize=TRUE){
	for(i in 1:length(listTrees)){
		tmpDat <- cbind(data[listOOB[[i]],], 1, 0)
		newTree <- getNewTree(listTrees[[i]]$trees[[1]], set, n.var)
		newTree <- checkNewTree(newTree, n.var)
		if(is.null(newTree))
			Nb[,i] <- Nb[,i] - colSums(mat.cl[listOOB[[i]],] == 0)
		else{
			preds <- as.vector(eval.logreg(newTree, tmpDat))
			if(any(preds>1))
				stop("Some preds > 1. Please inform the author.")
			Nb[,i] <- Nb[,i] - colSums(preds == mat.cl[listOOB[[i]],])
		}
	}
	out <- rowSums(Nb) / B
	if(standardize){
		tmp <- rowSums((Nb-out)^2) + (B-length(listTrees)) * out^2
		out <- sqrt(B*(B-1)) * out / sqrt(tmp)
	}
	out
}

compPermSet3Fast <- function(listTrees, set, Nb, mat.cl, data, listOOB, n.var, B,
		standardize=TRUE, prob.case=0.5){
	for(i in 1:length(listTrees)){
		newdata <- cbind(data[listOOB[[i]],], 1, 0)
		newtree <- lapply(listTrees[[i]]$trees, getNewTree, set, n.var)
		newtree <- lapply(newtree, checkNewTree, n.var)
		idsIn <- !unlist(lapply(newtree, is.null))
		if(sum(idsIn) == 0){
			pred <- listTrees[[i]]$coef[1]
			pred <- exp(pred) / (1+exp(pred))
			if(pred> 1 | pred<0)
				stop("Something went wrong. Please inform the author.")
			pred <- (pred > prob.case) * 1
			Nb[,i] <- Nb[,i] - colSums(mat.cl[listOOB[[i]],] == pred)
		}
		else{
			listTrees[[i]]$trees <- newtree[idsIn]
			if(any(!idsIn)){
				listTrees[[i]]$coef <- listTrees[[i]]$coef[c(TRUE, idsIn)]
				listTrees[[i]]$ntrees <- c(sum(idsIn), sum(idsIn))
			}
			preds <- as.vector(predict(listTrees[[i]], newdata, 3))
			if(any(preds>1 | preds<0))
				stop("Something went wrong. Please inform the author.")
			preds <- preds > prob.case
			Nb[,i] <- Nb[,i] - colSums(preds == mat.cl[listOOB[[i]],])
		}
	}
	out <- rowSums(Nb) / B
	if(standardize){
		tmp <- rowSums((Nb-out)^2) + (B-length(listTrees)) * out^2
		out <- sqrt(B*(B-1)) * out / sqrt(tmp)
	}
	out
}


compPermSet3Rebuild <- function(listTrees, set, Nb, mat.cl, data, listOOB, n.var, B,
		listInBagg, standardize=TRUE, prob.case=0.5){
	for(i in 1:length(listTrees)){
		newdata <- cbind(data[listOOB[[i]],], 1, 0)
		olddata <- cbind(data[listInBagg[[i]],], 1, 0)
		newtree <- lapply(listTrees[[i]]$trees, getNewTree, set, n.var)
		newtree <- lapply(newtree, checkNewTree, n.var)
		idsIn <- !unlist(lapply(newtree, is.null))
		if(sum(idsIn)==0){
			mat.design <- matrix(1, nrow=nrow(olddata))
			mat.new <- matrix(1, nrow=nrow(newdata))
		}
		else{
			newtree <- newtree[idsIn]
			mat.design <- cbind(1, sapply(newtree, eval.logreg, olddata))
			mat.new <- cbind(1, sapply(newtree, eval.logreg, newdata))
		}
		for(j in 1:ncol(mat.cl)){
			coef <- glm.fit(mat.design, mat.cl[listInBagg[[i]], j],
				family=binomial())$coefficients
			if(any(is.na(coef)))
				coef[is.na(coef)] <- 0
			preds <- as.vector(mat.new %*% coef)
			preds <- exp(preds) / (1+exp(preds))
			if(any(preds>1 | preds<0))
				stop("Something went wrong. Please inform the author.")
			preds <- preds > prob.case
			Nb[j,i] <- Nb[j,i] - sum(preds == mat.cl[listOOB[[i]], j])
		}
	}
	out <- rowSums(Nb) / B
	if(standardize){
		tmp <- rowSums((Nb-out)^2) + (B-length(listTrees)) * out^2
		out <- sqrt(B*(B-1)) * out / sqrt(tmp)
	}
	out
}


	
















	
	