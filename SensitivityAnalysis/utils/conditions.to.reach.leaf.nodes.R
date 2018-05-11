
#Get conditions to reach leaf nodes
conditions.to.reach.leaf.nodes <- function(fit)
{
  fr <- fit$frame
  #To get the leaf node indices in the tree
  leaf.nodes <- rownames(fr[fr$var == "<leaf>", ]) %>% as.integer()
  conditions <- list()
  k <- 1
  
  for(i in leaf.nodes)
  {
    #To get the path to the leaf node i
    leaf.node.path <- path.rpart(fit, i, print.it = F) %>% unlist()
    leaf.node.path <- leaf.node.path[-1] #removing the root node
    con <- paste(leaf.node.path, collapse = " & ")
    conditions[[k]] <- con
    k <- k + 1
  }
  
  return(unlist(conditions))
}
