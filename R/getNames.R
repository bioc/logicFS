getNames<-function(vec.primes,col.names){
	n.col<-length(col.names)
	vec.primes<-gsub("X","XtendedName",vec.primes)
	coded<-paste("XtendedName",1:n.col,sep="")
	for(i in n.col:1)
		vec.primes<-gsub(coded[i],col.names[i],vec.primes)
	vec.primes
}

