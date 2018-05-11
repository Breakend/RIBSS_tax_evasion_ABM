R2.eqn<- function(df,cx,cy,r2.only=TRUE){
  names(df)[cx]<-"x"
  names(df)[cy]<-"y"
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  if(r2.only){
    eq <- substitute(italic(R)^2~"="~r2,list(r2 = format(summary(m)$r.squared, digits = 3)))
  }
  as.character(as.expression(eq));                 
}