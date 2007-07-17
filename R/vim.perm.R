vim.perm<-function(object,mu=0,n.perm=10000,n.subset=1000,adjust="bonferroni",rand=NA){
	require(genefilter)
	out<-check.mat.imp(object,mu=mu)
	mat.imp<-out$mat.imp
	vim<-out$vim
	stat<-rowttests(mat.imp)$statistic
	B<-ncol(mat.imp)
	n.subs<-unique(c(seq(0,n.perm,n.subset),n.perm))
	n.subs<-diff(n.subs)
	larger<-numeric(length(stat))
	if(!is.na(rand))
		set.seed(rand)
	for(i in 1:length(n.subs)){
		mat.perm<-matrix(sample(c(-1,1),n.subs[i]*B,replace=TRUE),B)
		out<-compLarger(mat.imp,mat.perm,stat,B)
		larger<-larger+out
	}
	rawp<-larger/sum(n.subs)
	if(adjust=="qvalue"){
		require(siggenes)
		pi0<-pi0.est(rawp)$p0
		adjp<-qvalue.cal(rawp,pi0)
	}
	else
		adjp<-p.adjust(rawp,method=adjust)
	vim$vim<-1-adjp
	tmp<-if(adjust=="none") "Unadjusted"  else paste(toupper(adjust),"Adjusted\n")
	vim$measure<-paste(tmp,"Permutation Based")
	vim$threshold<-0.95
	vim$mu<-mu
	vim
}



