triologreg <- function(bin, resp, weights, choice, nleaves=5, penalty=0, control=NULL, rand=NA, notBuiltStop=TRUE){
	logreg.storetree <- function(x){
        	i1 <- matrix(x[-(1:3)], ncol = 4, byrow = TRUE)
        	i2 <- length(i1[, 1])
        	i3 <- data.frame(1:i2, i1)
        	names(i3) <- c("number", "conc", "knot", "neg", "pick")
        	i3
    	}
	wgt <- rep(weights, e=4)
	seed <- ifelse(is.na(rand), 0, rand)
	binnames <- colnames(bin)
	mdl <- 9
	n1 <- length(resp)
    	n2 <- ncol(bin)
    	nsep <- 0
	sep <- 0
	nrep <- 25
    	tree.control <- list(treesize=2^floor(log2(control$treesize)), opers=control$opers, 
		minmass=control$minmass)
  	cens <- rep(1, n1)
    	if(choice != 2){
		ntr <- c(1, 1)
        	msz <- rep(nleaves, 2)
	}
    	else{
		ntr <- c(1, 1)
		msz <- nleaves
	}
    	anneal.control <- list(start=control$start, end=control$end, iter=control$iter, 
		earlyout=control$earlyout, update=control$update)
	niter <- ifelse(control$iter==0, 50000, control$iter)
	mc.control <- list(nburn=control$nburn, niter=niter, hyperpars=c(control$hyperpars, 0, 0, 0),
		update=control$update, output=control$output)
	if(choice==7)
		anneal.control$update <- mc.control$update
	if(!is.na(rand))
		xseed <- rand
	else
        	xseed <- floor(runif(1) * 1e+06) + 1
	ipars <- c(mdl, msz[1:2], tree.control$treesize, ntr[1:2], tree.control$opers, 
		anneal.control$update, xseed, nrep, choice, anneal.control$earlyout, 
		tree.control$minmass, mc.control$nburn, mc.control$niter, mc.control$output)
	rpars <- c(anneal.control$start, anneal.control$end, anneal.control$iter, 
		penalty, mc.control$hyperpars)
	orders <- order(rank(resp) + runif(n1)/100)
	nkn <- tree.control$treesize * 2 - 1
    	nxx <- 2
    	if(choice == 1){
		n.a <- ntr[1] * (nkn * 4 + 3)
        	n.b <- nsep + ntr[1] + 1
        	n.c <- 2
    	}
    	if(choice == 2){
        	n.d <- (ntr[2] - ntr[1] + 1) * (msz[2] - msz[1] + 1)
        	n.a <- n.d * ntr[2] * (nkn * 4 + 3)
		n.b <- n.d * (nsep + ntr[2] + 1)
        	n.c <- n.d
    	}
    	if(choice == 6){
        	n.d <- msz[2] + 2
		n.a <- n.d * ntr[2] * (nkn * 4 + 3)
        	n.b <- n.d * (nsep + ntr[2] + 1)
        	n.c <- n.d
    	}
	n100 <- -100
	if(choice == 7){
		n.a <- 256
        	n.b <- n2
        	n.c <- n2 * n2
        	if(abs(mc.control$output) < 2) 
            		n.c <- 1
        	nxx <- n.c * n2
        	if(abs(mc.control$output) < 3) 
            		nxx <- 1
		n100 <- 0
    	}
	xtree <-  rep(n100, n.a)
	ip4 <- 2 * ipars[4] + 1
	fit <- .Fortran("slogreg", as.integer(n1), as.integer(n2), 
		as.integer(nsep), ip = as.integer(ipars), as.single(rpars), 
		as.single(t(sep)), as.integer(cens), as.integer(orders), 
		as.single(resp), as.single(wgt), as.integer(t(bin)), 
		trees = as.integer(xtree), coef = as.single(rep(n100, n.b)), 
		scores = as.single(rep(n100, n.c)), as.integer(ipars[6]), 
		as.integer(ip4), as.integer(rep(0, 2 * ipars[6] * ip4 * n1)), 
		as.integer(rep(0, 7 * ipars[6] * (ip4 + 1) * n2 * 4)), 
		as.single(rep(0, 7 * ipars[6] * (ip4 + 1) * n2 * 4)), 
		as.integer(t(bin)),rd4 = as.integer(rep(0,nxx)),
		PACKAGE = "LogicReg")
	type <- "Trio Logic Regression"
	if(choice %in% (1:2) && is.na(fit$coef[2])){
		if(notBuiltStop)
			cat("For some unknown reason, the fitting of the trio logic regression model failed.\n",
				"Please rerun the analysis with trioLR (maybe after restarting R).\n",
				"If this will not work, please contact the maintainer of the trio package.\n\n", sep="")
		else
			return(NULL)
	}
    	if(choice == 1) 
        	chs <- "single.model"
    	if(choice == 2) 
        	chs <- "multiple.models"
    	if(choice == 7) 
        	chs <- "Bayesian"
    	if(choice == 6) 
        	chs <- "greedy"
	tree.control$operators <- switch(tree.control$opers, "both", "and", "or", "both")
	m1 <- list(nsample = n1, nbinary = n2, nseparate = nsep, type = type, select = chs, 
		seed = rand, choice = choice, control=control)
    	if(choice == 7){
		v1 <- fit$trees
        	v3 <- 1:length(v1)
        	v3 <- max(v3[v1 > 0])
        	v1 <- v1[1:v3]
        	v1 <- data.frame(0:(v3 - 1), v1)
        	names(v1) <- c("size", "number")
        	m1$size <- v1
        	m1$single <- fit$coef
        	m1$single[m1$single < 0] <- 0
        	m1$mc.control <- mc.control
        	if(abs(mc.control$output) > 1){
            		m1$double <- matrix(fit$scores, ncol = length(fit$coef))
            		m1$double[m1$double < 0] <- 0
        	}
        	if(abs(mc.control$output) > 2){
            		m1$triple <- array(fit$rd4, dim = c(length(fit$coef), 
                		length(fit$coef), length(fit$coef)))
		}
	}
    	if(choice == 1){
        	m1$nleaves <- msz[1]
        	m1$ntrees <- ntr[1]
    	}	
    	else{
        	m1$nleaves <- msz
        	m1$ntrees <- ntr
    	}
    	m1$response <- resp
    	m1$binary <- bin
    	m1$separate <- sep
    	m1$censor <- cens
    	m1$weight <- wgt
	if(choice == 1){
		m1$penalty <- penalty
        	m2 <- list()
        	m2$ntrees <- ntr
        	m2$nleaves <- msz
        	m2$score <- fit$scores[1]
        	m2$coef <- fit$coef[1:(nsep + ntr[1] + 1)]
        	class(m2) <- "logregmodel"
        	lx <- 3 + 4 * nkn
        	m2$trees <- list()
        	for(i in 1:ntr[1]){
            		m3 <- list()
            		m3$whichtree <- i
            		m3$coef <- fit$coef[1 + nsep + i]
            		m3$trees <- logreg.storetree(fit$trees[(i - 1) * lx + (1:lx)])
            		class(m3) <- "logregtree"
            		m2$trees[[i]] <- m3
        	}
        	m1$model <- m2
    	}
    	if(choice == 6){
        	m1$nmodels <- fit$ip[1]
        	m1$alltrees <- list()
        	m1$allscores <- matrix(0, nrow = m1$nmodels, ncol = 3)
        	j <- 0
        	k <- 0
        	for(i in 1:m1$nmodels){
            		m2 <- list()
            		m2$score <- fit$scores[i]
            		m2$nleaves <- fit$trees[j + 1]
            		m2$ntrees <- fit$trees[j + 2]
            		m1$allscores[i, ] <- c(fit$scores[i], fit$trees[j + (1:2)])
            		m2$coef <- fit$coef[k + (1:(m2$ntrees + nsep + 1))]
            		class(m2) <- "logregmodel"
            		m2$trees <- list()
            		lx <- 3 + 4 * nkn
            		for(l in 1:m2$ntrees){
                		m3 <- list()
                		m3$whichtree <- l
                		m3$coef <- m2$coef[1 + nsep + l]
                		m3$trees <- logreg.storetree(fit$trees[j + (l - 1) * lx + (1:lx)])
                		class(m3) <- "logregtree"
                		m2$trees[[l]] <- m3
            		}
            		j <- j + lx * m2$ntrees
            		k <- k + (m2$ntrees + nsep + 1)
            		m1$alltrees[[i]] <- m2
        	}
    	}
    	if(choice == 2){
        	m1$nmodels <- fit$ip[1]
        	m1$alltrees <- list()
        	m1$allscores <- matrix(0, nrow = m1$nmodels, ncol = 3)
        	j <- 0
        	k <- 0
        	for(i in 1:m1$nmodels){
            		m2 <- list()
            		m2$score <- fit$scores[i]
            		m2$nleaves <- fit$trees[j + 1]
            		m2$ntrees <- fit$trees[j + 2]
            		m1$allscores[i, ] <- c(fit$scores[i], fit$trees[j + (1:2)])
            		m2$coef <- fit$coef[k + (1:(m2$ntrees + nsep + 1))]
            		class(m2) <- "logregmodel"
            		m2$trees <- list()
            		lx <- 3 + 4 * nkn
            		for(l in 1:m2$ntrees){
                		m3 <- list()
                		m3$whichtree <- l
                		m3$coef <- m2$coef[1 + nsep + l]
                		m3$trees <- logreg.storetree(fit$trees[j + (l - 1) * lx + (1:lx)])
                		class(m3) <- "logregtree"
                		m2$trees[[l]] <- m3
            		}
            		j <- j + lx * m2$ntrees
            		k <- k + (m2$ntrees + nsep + 1)
            		m1$alltrees[[i]] <- m2
        	}
    	}
    	m1$binnames <- binnames
    	m1
}
