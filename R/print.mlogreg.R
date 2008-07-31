print.mlogreg <- function(x,digits=3,...){
	models<-x$model
	levs<-levels(x$cl)
	ntrees<-x$ntrees
	nleaves<-x$nleaves
	cat("Multinomial Logic Regression\n\n",
		"Search Algorithm: ", ifelse(x$fast,"Stepwise Greedy Selection",
		"Simulated Annealing"),"\n",
		"Max. Number of Trees:  ",if(nleaves>9) " ",ntrees,"\n", 
		"Max. Number of Leaves: ",nleaves, "\n", 
		"Number of Levels: ",length(models)+1,"\n",
		"Reference Level:  ",levs[1],"\n", sep="")
	for(i in 1:length(models)){
		tmp<-models[[i]]
		trees<-sapply(tmp$tree,getTree)
		trees<-c("(Intercept)",trees)
		tab<-data.frame(Coefficient=tmp$coef,"Logic Tree"=trees,check.names=FALSE)
		rownames(tab)<-0:(nrow(tab)-1)
		if(any(trees==""))
			tab<-tab[trees!="",]
		cat("\n","Level: ",names(models)[i],"\n",sep="")
		print(format(tab,digits=digits))
	}
}