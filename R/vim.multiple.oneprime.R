vim.multiple.oneprime<-function(oneprime,cl,oob,prob.case=0.5){
	cl.train<-cl[-oob]
	coef.null<-log(sum(cl.train==1)/sum(cl.train==0))
	correct.null<-if(coef.null>prob.case) sum(cl[oob]) else sum(1-cl[oob])
	dat<-data.frame(cl=cl,x=oneprime)
	glm.out<-glm(cl~.,data=dat[-oob,],family="binomial")
	preds<-predict(glm.out,dat[oob,],type="response")>prob.case
	correct.prime<-sum(preds==cl[oob])
	correct.prime-correct.null
}

