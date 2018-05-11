
#############################################################
###        Create the Network          
#############################################################
create.network <- function(ave.degree, N)
{
  phi <- ave.degree/(N-1)
  g <- erdos.renyi.game(N, phi)
  
  return(g)
}