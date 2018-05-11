### this function depends on having the Portland netwotk data loaded up.

generate.random.income<- function(Household.Income.Cat,family.income.distrubution){
  
  HIC<-as.numeric(as.character(Household.Income.Cat))
  income <- NA*HIC
  income[HIC%in%14] <- 0
  
  
  family.income.distrubution.under.60000 <- 
    family.income.distrubution[family.income.distrubution$H.income.LB<60000,]
  
  for(Income.Cat in 1:12){
    those.Income.Cat <- HIC%in%Income.Cat 
    
    H.income.LB <-family.income.distrubution.under.60000[Income.Cat,"H.income.LB"]
    H.income.UB <- family.income.distrubution.under.60000[Income.Cat,"H.income.UB"]
    H.income.mean <- family.income.distrubution.under.60000[Income.Cat,"Mean.Income"]
    
    side <- sample(c(FALSE,TRUE), sum(those.Income.Cat), replace = TRUE)
    L.income <- runif(sum(side%in%FALSE),min=H.income.LB,max=H.income.mean)
    H.income <- runif(sum(side%in%TRUE),min=H.income.mean,max=H.income.UB)
    
    tmp <- income[those.Income.Cat]
    tmp[!(side)] <- round(L.income,0)
    tmp[side] <- round(H.income,0)
    income[those.Income.Cat]<- tmp
  }
  
  those.Income.Cat13 <- HIC%in%13
  
  family.income.distrubution.over.60000 <- 
    family.income.distrubution[family.income.distrubution$H.income.LB>=60000,]
  family.income.distrubution.over.60000$proprtion <- 
    family.income.distrubution.over.60000$number/
    sum(family.income.distrubution.over.60000$number)
  
  high.income.cat <- 1:nrow(family.income.distrubution.over.60000) 
  
  rownames(family.income.distrubution.over.60000) <- high.income.cat
  
  sample.high.income.cat<- sample(high.income.cat,sum(those.Income.Cat13), 
                                  replace = TRUE,
                                  prob = family.income.distrubution.over.60000$proprtion)
  
  Income.Cat13 <- income[those.Income.Cat13]
  prob<- c(0.52,0.48)
  for(Income.Cat in high.income.cat){
    
    if(Income.Cat>28) {prob<- c(0.9,0.1)}
    
    those.Income.Cat <- sample.high.income.cat%in%Income.Cat 
    
    H.income.LB <-family.income.distrubution.over.60000[Income.Cat,"H.income.LB"]
    H.income.UB <- family.income.distrubution.over.60000[Income.Cat,"H.income.UB"]
    H.income.mean <- family.income.distrubution.over.60000[Income.Cat ,"Mean.Income"]
    
    side <- sample(c(FALSE,TRUE), sum(those.Income.Cat), replace = TRUE, prob=prob)
    L.income <- runif(sum(side%in%FALSE),min=H.income.LB,max=H.income.mean)
    H.income <- runif(sum(side%in%TRUE),min=H.income.mean,max=H.income.UB)
    
    tmp <- Income.Cat13[those.Income.Cat]
    tmp[!(side)] <- round(L.income,0)
    tmp[side] <- round(H.income,0)
    Income.Cat13[those.Income.Cat]<- tmp
  }
  
  income[those.Income.Cat13]<- Income.Cat13
#   
#   income.plot<-ggplot(as.data.frame(income)) +
#     geom_histogram(aes(x=income,y=100*..count../sum(..count..)),bins = 100,color="black",fill="lightblue") +
#     theme_bw() +
#     xlim(0,500000)+
#     xlab("Income ($)") +
#     ylab("Percent of Households")+
#     ggtitle(paste("Histogram of network income",sep=""))+
#     theme(axis.text.x=element_text(size=16 ), 
#           axis.text.y=element_text(size=16 ) ,
#           title = element_text( size=18 ) ,
#           strip.text=element_text( size=12 ) ,
#           axis.title.x = element_text( size=18 ) ,
#           axis.title.y = element_text( size=18 ),
#           legend.text = element_text( size = 12))
#   
#   print(income.plot)
#   ggsave(income.plot,
#          file=paste(figures.dir,"Porland_HouseHold_Income_dist",
#                     length(income),".pdf",sep=""),  width=10, height=5)
  
  return(income)
}