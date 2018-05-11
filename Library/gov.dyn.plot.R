

gov.dyn.plot <- function(gov.dyn, small = 14,big = 18,
                         font.reduction.factor=0.75,
                         starting.year=2016) {
  
  if("psych" %in% (.packages())){
    detach("package:psych", unload=TRUE) 
  }
  
  n.yrs <- dim(gov.dyn)[1]
  gov.dyn[, 't'] <- (starting.year-1)+1:n.yrs
  
  #############################################################
  ###  Aggregates plot 1.     
  #############################################################
  
  sub.dyn <- gov.dyn[, c("US.government.expenses","total.revenue", "IT.revenue", "US.deficit")]/10^12
  max.y <- round(2*max(sub.dyn),0)/2
  sub.dyn$t <- gov.dyn$t 
  #names(sub.dyn) <- c("t", "Total Revenue", "Income Tax Revenue", "Marginal Deficit", "Audit Costs", "Taxes Recovered")
  melt.gov.dyn <- melt(sub.dyn,id.vars = "t",variable.name = "agg_variable", 
                              value.name = "agg_value")
  levels(melt.gov.dyn$agg_variable)<- c("Government Expenses", "Total Revenues",
                                        "Income Tax Revenues", "Deficit")
  
  
  agg.summary.plot <- 
    ggplot(melt.gov.dyn)+
    geom_line(aes(x=t, y=agg_value, group= agg_variable, colour = agg_variable),size=1.5)+
    #facet_wrap(~ agg_variable)+
    xlab("Year")+
    ylab("Dollars (Trillions)")+
    scale_x_continuous(breaks = round(seq(min(sub.dyn$t), max(sub.dyn$t), by = 10),1)) +
    scale_y_continuous(limits= c(0,max.y),breaks = round(seq(0, max.y, by =1),1)) +
    scale_color_manual(name="agg_variable",values=c("red","steelblue3","darkolivegreen3","purple"))+
    theme_bw() + 
    theme(legend.title = element_blank(),
        legend.position=c(0.55, 0.60),
        legend.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        axis.text.x=element_text(size=small,face="italic",angle=35),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y=element_text(size=small,face="italic" ) ,
        strip.text=element_text( size=small,face="italic" ) ,
        axis.title.x = element_text( size=big,face="italic" ) ,
        axis.title.y = element_text( size=big ,face="italic"),
        legend.text = element_text( size = small,face="italic"))
  
  
  #############################################################
  ###  Audit cost versus recovered tax revenues.     
  #############################################################
  
  sub.dyn <- gov.dyn[, c("audit.costs", "US.past.taxes.to.pay")]/10^9
  max.y <- round(max(sub.dyn)/5,0)*5
  sub.dyn$t <- gov.dyn$t 
  
  quick.ma<- function(x,n){
    R<-stats::filter(x, rep(1/n,n), sides=1)
    for(i in n:1){
      R[i] <- stats::filter(x[1:i], rep(1/i,i), sides=1)[i]
    }
    return(as.numeric(R))
  }
  
  sub.dyn$ma.audit.costs <- quick.ma(sub.dyn$audit.costs,10)
  sub.dyn$ma.US.past.taxes.to.pay <- quick.ma(sub.dyn$US.past.taxes.to.pay,10)
  
  sub.dyn <- sub.dyn[,c("t","ma.audit.costs","ma.US.past.taxes.to.pay")]
  
  melt.gov.dyn <- melt(sub.dyn,id.vars = "t",variable.name = "agg_variable", 
                       value.name = "agg_value")
  levels(melt.gov.dyn$agg_variable)<- c("Audit Costs", "Recovered Tax Revenues")
  
     audit.costs.and.recoved.tax.rev.plot <- 
    ggplot(melt.gov.dyn)+
    geom_line(aes(x=t, y=agg_value, group= agg_variable, colour = agg_variable),size=1.5)+
    #facet_wrap(~ agg_variable)+
    xlab("Year")+
    ylab("Dollars (Billions)")+
    scale_color_manual(name="agg_variable",values=c("red","steelblue3"))+
    scale_x_continuous(breaks = round(seq(min(sub.dyn$t), max(sub.dyn$t), by = 10),1)) +
    scale_y_continuous(limits= c(0,max.y),breaks = round(seq(0, max.y, by = 10),1)) +
    theme_bw() + 
    theme(legend.title = element_blank(),
          legend.position=c(0.55, 0.75),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic",angle=35),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y=element_text(size=small,face="italic" ) ,
          strip.text=element_text( size=small,face="italic" ) ,
          axis.title.x = element_text( size=big,face="italic" ) ,
          axis.title.y = element_text( size=big ,face="italic"),
          legend.text = element_text( size = small,face="italic"))

     #############################################################
     ###  compliance deficit plot     
     #############################################################
     
     sub.dyn <- gov.dyn[, c("US.marginal.Debt","US.marginal.deficit")]/10^9
     sub.dyn$compliance <- (100-gov.dyn$US.tax.gap)/100
     max.y <- round(max(sub.dyn)/5,0)*5
     sub.dyn$t <- gov.dyn$t 
     
     AXIS1_MIN_1 <- 0.95*min(sub.dyn$compliance)
     AXIS1_MAX_1 <- 1.05*max(sub.dyn$compliance)
     AXIS2_MIN_1 <- min(sub.dyn$US.marginal.deficit)
     AXIS2_MAX_1 <- 1.1*max(sub.dyn$US.marginal.deficit)
     
     scale_to_value <- function(values,to) rescale(values, to = to)
     
     compliance.deficit.plot <-
       ggplot(sub.dyn, aes(x=t))+
       geom_line(aes(y=compliance, colour = "Compliance"),size=1.5)+
       geom_line(aes(y=scale_to_value(US.marginal.deficit,to = c(AXIS1_MIN_1, AXIS1_MAX_1)),
                     colour = "Marginal Deficit"),size=1.5,
                 alpha=0.75)+
       scale_y_continuous(limits=c(AXIS1_MIN_1, AXIS1_MAX_1),labels = scales::percent,
                          sec.axis = sec_axis( ~ scale_to_value(.,to = c(AXIS2_MIN_1, AXIS2_MAX_1)), 
                                               name = "Marginal Deficit\n since 2016 (billions)" ,
                                               #expression(italic(paste(Delta, "Debt since 2016")))
                                               labels = scales::dollar)
       )+
       scale_color_manual(name="group",values=c("steelblue3","red"))+
       xlab("Year")+
       ylab("Voluntary Compliance\n Rate (%)")+
       scale_x_continuous(breaks = round(seq(min(sub.dyn$t), max(sub.dyn$t), by = 10),1)) +
       theme_bw() + 
       theme(legend.title = element_blank(),
             legend.position="none",#c(0.25, 0.75),
             legend.background = element_rect(fill="transparent"),
             panel.border = element_blank(),
             axis.text.x=element_text(size=small,face="italic",angle=35), 
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             axis.text.y=element_text(size=small,face="italic" ,color = "steelblue3") ,
             axis.text.y.right = element_text(color = "red"),
             strip.text=element_text( size=small,face="italic" ) ,
             axis.title.x = element_text( size=big,face="italic") ,
             axis.title.y = element_text( size=big ,face="italic",color = "steelblue3"),
             axis.title.y.right = element_text( size=big ,face="italic",color = "red"),
             legend.text = element_text( size = small,face="italic"))   
     
       
  
  #############################################################
  ###  compliance debt plot     
  #############################################################
  
  sub.dyn <- gov.dyn[, c("US.marginal.Debt","US.marginal.deficit")]/10^9
  sub.dyn$compliance <- (100-gov.dyn$US.tax.gap)/100
  max.y <- round(max(sub.dyn)/5,0)*5
  sub.dyn$t <- gov.dyn$t 
  
  AXIS1_MIN_2 <- 0.95*min(sub.dyn$compliance)
  AXIS1_MAX_2 <- 1.05*max(sub.dyn$compliance)
  AXIS2_MIN_2 <- min(sub.dyn$US.marginal.Debt)
  AXIS2_MAX_2 <- 1.1*max(sub.dyn$US.marginal.Debt)
  
  
  compliance.debt.plot <-
  ggplot(sub.dyn, aes(x=t))+
    geom_line(aes(y=compliance, colour = "Compliance"),size=1.5)+
    geom_line(aes(y=scale_to_value(US.marginal.Debt,to = c(AXIS1_MIN_2, AXIS1_MAX_2)), colour = "Delta Debt"),size=1.5,alpha=0.75)+
    scale_y_continuous(limits=c(AXIS1_MIN_2, AXIS1_MAX_2),labels = scales::percent,
                       sec.axis = sec_axis( ~ scale_to_value(.,to = c(AXIS2_MIN_2, AXIS2_MAX_2)), 
                                    name = "Change in Compliance Debt\n since 2016 (billions)" ,
                                    #expression(italic(paste(Delta, "Debt since 2016")))
                                    labels = scales::dollar)
    )+
    scale_color_manual(name="group",values=c("steelblue3","red"))+
    xlab("Year")+
    ylab("Voluntary Compliance\n Rate (%)")+
    scale_x_continuous(breaks = round(seq(min(sub.dyn$t), max(sub.dyn$t), by = 10),1)) +
    theme_bw() + 
    theme(legend.title = element_blank(),
          legend.position="none",#c(0.25, 0.75),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic",angle=35), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y=element_text(size=small,face="italic" ,color = "steelblue3") ,
          axis.text.y.right = element_text(color = "red"),
          strip.text=element_text( size=small,face="italic" ) ,
          axis.title.x = element_text( size=big,face="italic") ,
          axis.title.y = element_text( size=big ,face="italic",color = "steelblue3"),
          axis.title.y.right = element_text( size=big ,face="italic",color = "red"),
          legend.text = element_text( size = small,face="italic"))
  
  
  #############################################################
  ###  compliance vs debt to gdp     
  #############################################################
  
  sub.dyn <- gov.dyn[, c("t","ratio")]
  sub.dyn$compliance <- (100-gov.dyn$US.tax.gap)/100
  
  AXIS1_MIN_3 = 0.9*min(sub.dyn$compliance)
  AXIS1_MAX_3 = 1.1*max(sub.dyn$compliance)
  AXIS2_MIN_3 = 0.9*min(sub.dyn$ratio)
  AXIS2_MAX_3 = 1.1*max(sub.dyn$ratio)
  
  
  
  compliance.debt.to.gdp.plot <-
    ggplot(sub.dyn, aes(x=t))+
    geom_line(aes(y=compliance, colour = "Compliance"),size=1.5)+
    geom_line(aes(y=scale_to_value(ratio,to = c(AXIS1_MIN_3, AXIS1_MAX_3)), colour = "Ratio"),size=1.5)+
    scale_y_continuous(limits=c(AXIS1_MIN_3, AXIS1_MAX_3),labels = scales::percent,
                       sec.axis = sec_axis( ~ scale_to_value(.,to = c(AXIS2_MIN_3, AXIS2_MAX_3)), 
                                            name = "Transformed Debt/GDP ratio" )
    ) +
    scale_color_manual(name="group",values=c("steelblue3","red"))+
    xlab("Year")+
    ylab("Voluntary Compliance\n Rate (%)")+
    scale_x_continuous(breaks = round(seq(min(sub.dyn$t), max(sub.dyn$t), by = 10),1)) +
    theme_bw() + 
    theme(legend.title = element_blank(),
          legend.position="none",#c(0.25, 0.75),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic",angle=35),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y=element_text(size=small,face="italic" ,color = "steelblue3") ,
          axis.text.y.right = element_text(color = "red"),
          strip.text=element_text( size=small,face="italic" ) ,
          axis.title.x = element_text( size=big,face="italic") ,
          axis.title.y = element_text( size=big ,face="italic",color = "steelblue3"),
          axis.title.y.right = element_text( size=big ,face="italic",color = "red"),
          legend.text = element_text( size = small,face="italic"))
  
  
  #############################################################
  ###  compliance vs deterrence.ratio.gains.to.costs   
  #############################################################
  
  sub.dyn <- gov.dyn[, c("US.penalty.and.past.tax.to.pay","audit.costs")]/10^9
  sub.dyn$ratio <- sub.dyn$US.penalty.and.past.tax.to.pay/sub.dyn$audit.costs
  sub.dyn$compliance <- (100-gov.dyn$US.tax.gap)/100
  sub.dyn$t <- gov.dyn$t 
  
  sub.dyn$ma.audit.costs <- quick.ma(sub.dyn$audit.costs,20)
  sub.dyn$ma.US.penalty.and.past.tax.to.pay <- quick.ma(sub.dyn$US.penalty.and.past.tax.to.pay,20)
  sub.dyn$ratio <- sub.dyn$ma.US.penalty.and.past.tax.to.pay/sub.dyn$ma.audit.costs
  
  AXIS1_MIN_4 = 0.9*min(sub.dyn$compliance)
  AXIS1_MAX_4 = 1.1*max(sub.dyn$compliance)
  AXIS2_MIN_4 = 0.9*min(sub.dyn$ratio)
  AXIS2_MAX_4 = 1.1*max(sub.dyn$ratio)
  
  
  deterrence.ratio.gains.to.costs.plot <-
    ggplot(sub.dyn, aes(x=t))+
    geom_line(aes(y=compliance, colour = "Compliance"),size=1.5)+
    geom_line(aes(y=scale_to_value(ratio,to = c(AXIS1_MIN_4, AXIS1_MAX_4)), colour = "Ratio"),size=1.5)+
    scale_y_continuous(limits=c(AXIS1_MIN_4, AXIS1_MAX_4),labels = scales::percent,
                       sec.axis = sec_axis( ~ scale_to_value(.,to = c(AXIS2_MIN_4, AXIS2_MAX_4)), 
                                            name = "Deterrence ratio of\n  Gains to Costs" )
    ) +
    scale_color_manual(name="group",values=c("steelblue3","red"))+
    xlab("Year")+
    ylab("Voluntary Compliance\n Rate (%)")+
    scale_x_continuous(breaks = round(seq(min(sub.dyn$t), max(sub.dyn$t), by = 10),1)) +
    theme_bw() + 
    theme(legend.title = element_blank(),
          legend.position="none",#c(0.25, 0.75),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_blank(),
          axis.text.x=element_text(size=small,face="italic",angle=35),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y=element_text(size=small,face="italic" ,color = "steelblue3") ,
          axis.text.y.right = element_text(color = "red"),
          strip.text=element_text( size=small,face="italic" ) ,
          axis.title.x = element_text( size=big,face="italic") ,
          axis.title.y = element_text( size=big ,face="italic",color = "steelblue3"),
          axis.title.y.right = element_text( size=big ,face="italic",color = "red"),
          legend.text = element_text( size = small,face="italic"))
  
  
  R<- list(  agg.summary.plot=agg.summary.plot,
             audit.costs.and.recoved.tax.rev.plot=audit.costs.and.recoved.tax.rev.plot,
             compliance.deficit.plot = compliance.deficit.plot,
             compliance.debt.plot = compliance.debt.plot,
             compliance.debt.to.gdp.plot =compliance.debt.to.gdp.plot ,
             deterrence.ratio.gains.to.costs.plot=deterrence.ratio.gains.to.costs.plot) 
  
  return(R)
}