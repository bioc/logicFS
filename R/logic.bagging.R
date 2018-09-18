logic.bagging <- function(x,...) UseMethod("logic.bagging")


logic.bagging.formula <- function(formula,data,recdom=TRUE,...){
	xy<-getXy(formula,data,recdom=recdom)
	out<-logic.bagging(xy$x,xy$y,...)
	out$facInfo<-xy$facInfo
	out
}



logic.bagging.default <-
function (x, y, B = 100, useN = TRUE, ntrees = 1, nleaves = 8, 
	glm.if.1tree = FALSE, replace = TRUE, sub.frac = 0.632, anneal.control = logreg.anneal.control(), 
	oob = TRUE, onlyRemove = FALSE, prob.case = 0.5, importance = TRUE, 
	score = c("DPO", "Conc", "Brier", "PL"),  addMatImp = FALSE, fast = FALSE, 
	neighbor = NULL, adjusted = FALSE, ensemble = FALSE, rand = NULL, ...) 
  {
    if (!is.matrix(x)) 
      stop("x must be a matrix.")
    if (any(is.na(x))) 
      stop("No missing values allowed.")
    if (any(is.na(y))) 
      stop("No missing values allowed.")
    if (any(!x %in% c(0, 1))) 
      stop("Some of the values of the predictors are not 0 or 1.")
    if (!is.numeric(y) & !is.factor(y)) 
      stop("The response must be either numeric or a factor.")
    if (class(y) != "Surv"){
      n <- length(y)
      le.uni <- length(unique(y))
    } else { 
      if (!(attributes(y)$type == "right"))
        stop("Survival data must be right censored.")
      if (replace)
        warning("Bootstrapping produces a lot of ties in the data.\n", 
                "Better use subsampling.")
      n <- length(y[,1])
      le.uni <- length(unique(y[,1]))
      cens <- y[, 2]
    }
    if (is.factor(y)){
      if (le.uni == 2) 
        y <- as.numeric(y) - 1
      else {
        FUN <- mlogreg
        type <- 9
        select <- ifelse(fast, 0, 1)
      }
    }
    if (is.numeric(y)){
      FUN <- logreg
      select <- ifelse(fast, 6, 1)
      if (le.uni < 2) 
        stop("The response is constant.")
      if (le.uni == 2){
        if (any(!y %in% c(0, 1))) 
          stop("Some of the values of the response are not 0 or 1.")
        type <- ifelse(ntrees > 1 | glm.if.1tree, 3, 1)
      }
      else {
        if (le.uni < min(10, n/2)){
          if (class(y) == "Surv")
             warning("Large number of ties in data", call. = FALSE) 
          else 
             stop("The response seems to be neither binary nor continuous.\n", 
                      "If the response is nominal, please use as.factor(y).")
        }
        if (class(y) == "Surv"){
          type <- 4
        }
        else type <- 2
      }
    }
    if (is.null(colnames(x))){
      colnames(x) <- paste("Var", 1:ncol(x), sep = "")
      warning("Since x has no column names, generic ones are added.", 
              call. = FALSE)
    }
    if (n != nrow(x)) 
      stop("The length of y must be equal to the number of rows of x.")
    if (!replace){
      if (sub.frac < 0.1 | sub.frac > 0.9) 
        stop("sub.frac must be between 0.1 and 0.9.")
      n.sub <- ceiling(sub.frac * n)
      sampling <- paste("Subsampling (", 100 * sub.frac, "% of the observations)", 
                        sep = "")
    } else sampling <- "Bagging"
    
    list.trees <- list.bagg <- vector("list", B)
    if (!is.null(rand)) 
      set.seed(rand)
    for (i in 1:B){
      bagg <- if (replace) 
        sample(n, n, replace = TRUE)
      else sample(n, n.sub)
      if (class(y) != "Surv"){
        tmp.out <- FUN(y[bagg], x[bagg, ], type = type, select = select, 
                       ntrees = ntrees, nleaves = nleaves, anneal.control = anneal.control)
      } else tmp.out <- FUN(y[bagg,1], x[bagg, ], cens = cens[bagg], type = type, select = select, 
                             ntrees = ntrees, nleaves = nleaves, anneal.control = anneal.control)
      
      if (!fast | type == 9) 
        list.trees[[i]] <- tmp.out$model
      else {
        ids.min <- which.min(tmp.out$allscores[, 1])
        list.trees[[i]] <- tmp.out$alltrees[[ids.min]]
      }
      list.bagg[[i]] <- bagg
    }
    log.out <- list(logreg.model = list.trees, inbagg = list.bagg, 
                    data = x, type = type, ntrees = ntrees, nleaves = nleaves, 
                    cl = y, oob.error = NULL, vim = NULL, sampling = sampling, 
                    fast = fast)
    class(log.out) <- "logicBagg"
    if (oob) 
      log.out$oob.error <- logic.oob(log.out, prob.case = prob.case)
    if (importance){
      log.out$vim <- vim.logicFS(log.out, neighbor = neighbor, adjusted = adjusted, 
                                 useN = useN, onlyRemove = onlyRemove, prob.case = prob.case, 
                                 score = score, ensemble = ensemble, addMatImp = addMatImp)
    } 
    log.out
  }
