get.ALP.net.degree<- function(x,subset,talk.taxes =T,show.summary.instead=F){
  if(talk.taxes){
    r<- x[subset,3]
  }else{
    r<- x[subset,2]
  }
  r <- summaryfunctionFull(r)
  if(!show.summary.instead){
    r <- r[!(names(r)%in%"NA's")]
    r<- c(LB = min(r[r>0]),
    Mean= as.numeric(r["Mean"] ),
    UB = max(r[r<r["Max."]]))
    }
  return(r)
}