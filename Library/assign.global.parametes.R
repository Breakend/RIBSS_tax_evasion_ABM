assign.global.parametes<- function(dummy){
## note need to do eval(assign.global.parametes(x)) when calling this function.
## Do not use this function with as.vector.forced fn. 
rr<-NULL
for(ii in 1:length(names(dummy))){
rr<-c(rr,parse(text=paste(names(dummy[ii]),"<-as.numeric(",deparse(substitute(dummy)),"[",as.character(ii),"])",sep=""))) }
return(rr)
} 