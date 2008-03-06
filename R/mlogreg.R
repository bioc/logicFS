`mlogreg` <-
function(x,...) UseMethod("mlogreg")


`mlogreg.formula` <-
function(formula,data,recdom=TRUE, ...){
	xy<-getXy(formula,data,recdom=recdom)
	out<-mlogreg(xy$x,xy$y,...)
	out$facInfo<-xy$facInfo
	out
}


`mlogreg.default` <-
function(x,y,ntrees=1,nleaves=8,anneal.control=logreg.anneal.control(),
		rand=NA,...){
	require(LogicReg)
	if(!is.matrix(x))
		stop("x must be a matrix.")
	if(any(is.na(x)))
		stop("No missing values allowed.")
	if(any(!x%in%c(0,1)))
		stop("All variables in x must be binary with values 0 and 1.")
	if(any(is.na(y)))
		stop("No missing values allowed.")
	if(length(y)!=nrow(x))
		stop("The length of y must be equal to the number of rows of x.")
	tab<-table(y)
	if(length(tab)>9)
		stop("y has more than 9 levels.")
	if(any(tab<5))
		stop("There must be at least 5 observations in each group.")
	if(!is.factor(y))
		y<-as.factor(y)
	levs<-levels(y)
	n.lev<-length(levs)
	if(n.lev==1)
		stop("y is constant.")
	list.logreg<-vector("list",n.lev-1)
	ids<-y==levs[1]
	if(!is.na(rand))
		set.seed(rand)
	for(i in 2:n.lev){
		ids2<-y==levs[i]
		tmp.mat<-x[ids | ids2, ]
		tmp.y<-(y[ids | ids2] == levs[i]) * 1
		list.logreg[[i-1]]<-logreg(resp=tmp.y,bin=tmp.mat,type=3,select=1,ntrees=ntrees,
			nleaves=nleaves,anneal.control=anneal.control)$model
	}
	names(list.logreg)<-levs[2:n.lev]
	out<-list(logreg.model=list.logreg,data=x,cl=y,ntrees=ntrees,nleaves=nleaves)
	class(out)<-"mlogreg"
	out
}

