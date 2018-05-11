ALP.glm<- function(df,dependent,covariates,threshold=0.05,family=gaussian(),lab,survey.output.dir,texreg.digits=2){
  
  NA.covariates <- paste(covariates,"NA.Indicator",sep="__")
  
  NA.covariates <-NA.covariates[NA.covariates%in%names(df)]
  
  covariates <- c(covariates,NA.covariates)
  
  covariates<- covariates[covariates%in%colnames(df)]
  
  formula <- paste(dependent, "~",paste(covariates,collapse= "+"))
  print(formula)
  formula <- as.formula(formula)
  
  gillo <- glm(formula, data = df, family = family,na.action = na.exclude )
  print(summary(gillo))
  
  tmp <- summary(gillo)$coeff[-1,4] < threshold
  significat.covariates <-(attr(gillo$terms , "term.labels"))[tmp]
  
  
  tmp <- strsplit(significat.covariates[grepl("NA.Indicator", significat.covariates)] ,"__")
  if(length(tmp)>0){
    for(j in 1:length(tmp)){
      if(!(tmp[[j]][1]%in% significat.covariates)){
      significat.covariates <- significat.covariates[!(significat.covariates%in%paste(tmp[[j]],collapse="__"))]
      }
    }
  }
  
  
  reg.out <- texreg(gillo,
                    caption=lab,
                    single.row=T, center=F,
                    dcolumn=FALSE,
                    override.se=summary(gillo)$coef[,2],
                    override.pval=summary(gillo)$coef[,4])
  
  print(reg.out,file=paste(survey.output.dir,gsub(" ","_",lab),".tex",sep=""))
  
  
  x<- df[,dependent]
  y <- df[,covariates]
  y <- df[,covariates[sapply(y, is.numeric)]]
  
  cor.tab<- cor(x,y,use="pairwise.complete.obs")
  if(length(cor.tab)>1){  
    rownames(cor.tab) <- dependent
    print(cor.tab)
  }
  
  
  if(length(significat.covariates)==0){
    print("no sig covariates")
  }
  
  
  if(length(significat.covariates)>0){
    
    x<- df[,dependent]
    y <- df[,significat.covariates]
    if(length(significat.covariates)>1){
      y <- df[,significat.covariates[sapply(y, is.numeric)]]
    }else{
      y <- df[,significat.covariates]
    }
    
    cor.tab<- cor(x,y,use="pairwise.complete.obs")
    if(length(cor.tab)>1){  
      rownames(cor.tab) <- dependent
      print(cor.tab)
      print(xtable(cor.tab),file=paste(survey.output.dir,gsub(" ","_",lab),"_cor.tex",sep=""))
    }
    
    
    sig.formula <- paste(dependent, "~",paste(significat.covariates,collapse= "+"))
    print(sig.formula)
    sig.formula<- as.formula(sig.formula)
    
    gillo.sig <- glm(sig.formula, data = df, 
                     family = family, na.action = na.exclude)
    print(summary(gillo.sig))
    
    
    lab<- paste( lab,"Sig", sep=" ") 
    reg.out <- texreg(gillo.sig,
                      caption=lab,
                      single.row=T, center=F,
                      dcolumn=FALSE,
                      digits = texreg.digits,
                      override.se=summary(gillo.sig)$coef[,2],
                      override.pval=summary(gillo.sig)$coef[,4])
    
    print(reg.out,file=paste(survey.output.dir ,gsub(" ","_", lab),".tex",sep=""))
  }else{
    gillo.sig<- gillo
  }
  
  return(gillo.sig)
}

