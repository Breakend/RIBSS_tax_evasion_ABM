#Function to calculate the odds ratios: n-n/n-c and c-c/c-n
#For complete reasoning see below

calculate.odds.ratios <- function(nodes, edges)
{
  edges[, 'from.status'] <- nodes[edges$from, 'group']
  edges[, 'to.status'] <- nodes[edges$to, 'group']
  
  total.edges <- dim(edges)[1]
  
  num.compl <- with(nodes, dim(nodes[group == 'Compliant', ])[1])
  num.non.compl <- with(nodes, dim(nodes[group == 'Non-Compliant', ])[1])
  total.num <- dim(nodes)[1]
      
  nn.model <- with(edges, dim(edges[from.status == 'Non-Compliant' & to.status == 'Non-Compliant', ])[1])
  cc.model <- with(edges, dim(edges[from.status == 'Compliant' & to.status == 'Compliant', ])[1])
  cn.model <- with(edges, dim(edges[from.status == 'Compliant' & to.status == 'Non-Compliant', ])[1])+with(edges, dim(edges[from.status == 'Non-Compliant' & to.status == 'Compliant', ])[1])
  
  #nn.random <- choose(num.non.compl, 2)/total.edges
  #cn.random <- num.non.compl*num.compl/total.edges
  cc.random <- choose(num.compl, 2)/choose(total.num, 2)
  nn.random <- choose(num.non.compl, 2)/choose(total.num, 2)
  cn.random <- 1- choose(num.non.compl, 2)/choose(total.num, 2)-
                  choose(num.compl, 2)/choose(total.num, 2)
  
  
  if(nn.random == 0 || cn.random == 0) {
    nn.over.nc <- 0
  } else {
    nn.over.nc <- (nn.model/cn.model)/(nn.random/cn.random)
  }
  
  if(cc.random == 0 || cn.random == 0) {
    cc.over.cn <- 0
  } else{
    cc.over.cn <- (cc.model/cn.model)/(cc.random/cn.random)
  }
  
  ratio.names <- c("NN/CN", "CC/CN")
  return(data.frame(measure = factor(ratio.names, levels = ratio.names), value = c(nn.over.nc, cc.over.cn)))
}


#Consider a fully connected network with N nodes. The number of edges is N(N-1)/2 or equally choose(N,2). 
#
#Consider half of the nodes be red and the other green. Intuitively, 25% of edges connect red to red, 
#25% green to green and 50% red-green.
#
#This is indeed the case because we have choose(N/2,2) edges connecting red-red which is equal to 
#25% of choose(N,2) (as N-> infinity).
#
#
#Now consider a random network (i.e., Erdos Renyi with no red and green bias in the way they connect) 
#that is not fully connected. This random network can be obtained by randomly selecting and removing a 
#certain proportion of edges from the fully connected network. Because there is equal probability of 
#selecting an edge to be removed – the ratio of 25% does not change. Thus we still get 25% red-red, 
#25% green-green and 50% the rest.
#
#Now let’s generalize.
#
#Lets say we have N nodes of which G are green and R are red nodes (N= G+R) and we have M edges, then if 
#the network is fully connected then M= choose(N,2) and we have a proportion of choose(R,2)/choose(N,2) red-red edges.
#As we decrease the number of edges from choose(N,2) to a lower number M in an unbiased random way, this proportion is unaltered. 
#

