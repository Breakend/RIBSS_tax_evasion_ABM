
require(visNetwork)

get.compliance.assortativity.measure<- function(g,track.dyn,
                                                field ="hideable.reported",
                                                threshold=0.02, mixing.matrix = F){
  tmp.dyn <- track.dyn[track.dyn$t == max(track.dyn$t), ]
  tmp.dyn$evaded <- (100- tmp.dyn[,field])/100
  V(g)$group <- "Non-Compliant"
  V(g)$group[tmp.dyn$evaded <=threshold] <- "Compliant"
  nam<- data.frame(measure="assortativity",value=assortativity_nominal(g, types=as.numeric(as.factor(V(g)$group)), directed = F))
  g.data <- toVisNetworkData(g)
  odds.ratios <- calculate.odds.ratios(g.data$nodes, g.data$edges)
  assortativity.measures <- rbind(odds.ratios,nam)
  mm <- NA
  
  if(mixing.matrix) {
    var.cat<-as.numeric(quantile(
      tmp.dyn[,field],
      prob=c(0:5)/5))
    if(any(duplicated(var.cat))){
      var.cat<- c(0,5,10,50,70,100)
      }else{
          var.cat[2:4]<-c(5,10,50)
          var.cat <- round(var.cat,0)
          var.cat<- var.cat[order(var.cat)]
      }
    
    
    var.cat[var.cat>10]<- round(var.cat[var.cat>10],-1)
    label.set<-paste("[",var.cat[-length(var.cat)],",",var.cat[-1],")",sep="")
    label.set[length(label.set)] <- gsub( ")", "]",label.set[length(label.set)]) 
    tmp.dyn$cat <- as.factor(findInterval(
      tmp.dyn[,field], var.cat,all.inside = T))
    levels(tmp.dyn$cat) <- label.set
    
    V(g)$cat <- as.character(tmp.dyn$cat) 
    mm<- get.mixmat(g, "cat",force.symmetry=T )
  }
  
  R <- list(assortativity.measures=assortativity.measures,
            mixing.matrix=mm)
  
  return(R)
}

##get.compliance.assortativity.measure(g,track.dyn)



  
