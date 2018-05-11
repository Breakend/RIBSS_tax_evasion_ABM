UniTri.Inputs <- function(param.row){

  Mode  <- as.numeric(param.row[1])
  Lower <- as.numeric(param.row[2])
  Upper <- as.numeric(param.row[3])
  PDF  <- param.row[4]
  
  if ((Mode == Upper) & (Mode == Lower)){
  return(c(Lower=Lower,Upper=Upper,Shape=1,Mode=Mode)) }
  if (Mode == Upper) Mode <- Mode - 0.001*(Upper-Lower)
  if (Mode == Lower) Mode <- Mode + 0.001*(Upper-Lower)
  
  if(PDF == "Tri" | PDF == "PERT" | PDF == "Beta")
  {
    ## get beta-PERT fit to triangular 
    ## see http://www.riskamp.com/library/pertdistribution.php
    lambda <- 4
    mu <- (Lower+Upper+lambda*Mode)/(lambda+2)
    shape1 <- (mu-Lower)*(2*Mode-Lower-Upper)/((Mode-mu)*(Upper-Lower))
    shape2 <- max(shape1 *(Upper-mu)/(mu-Lower),1.001)
    shape1 <- max(shape1,1.001)  
    Shape  <- shape2
    if(mu == Mode) Shape <- 2      
  }
  if(PDF == "Uni" | PDF == "") Shape <- 1
  
  return(c(Lower=Lower,Upper=Upper,Shape=Shape,Mode=Mode))
}