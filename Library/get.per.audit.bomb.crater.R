#### Bomb Crater Effect
get.per.audit.bomb.crater<-function(q,n,s.audit.discount, multip.factor0=0.58){
  multip.factor<-(multip.factor0-1)*exp(-(1-s.audit.discount)*n)
  q.new <- (multip.factor+1)*q
  return(q.new)
  return(R)
}

## NTA 2015 :Importantly, the results also indicate that audits have a detrimental long-term impact on the reporting behavior of taxpayers who do not experience an additional tax assessment. Three years after having undergone enforcement activity, these taxpayers report around 35% less in taxable income than the control group. The difference is significant at the 1% level.


### Kirchler, Maciejovsky and Schwarzenberger (2005) predicted that tax compliance depends on audit probability, sanctions and the time lag between past audits. These assumptions were tested in a laboratory experiment, where participants earned their income endogenously in a competitive market. Figure 15 of Kirchler, Erich (2007-06-21). The Economic Psychology of Tax Behaviour (p. 123). Cambridge University Press.: Compliance rate by sanctions and periods since last audit (Kirchler, Maciejovsky and Schwarzenberger, 2005) - we have  for 50% sanction rate the baseline compliance is 0.50, and 1 year and 3 year compliance is 0.31 and 0.32. Note that 0.5*(1-0.35)~ 0.32 consitent with t NTA report. In Figure 14: Compliance rate by audit probability and periods since last audit - we have we have a based line compliance of 0.57 and 1 year after an audit compliance falls to 0.33 with audit rist 0.3. This mean that after 1 year people report 42% less taxable income. 

####The bomb crater effect is a similar effect whereby immediately after the occurrence of an audit, compliance decreases\footnote{This was found by Mittone (2006) in a series of laboratory experiments on the dynamics of tax evasion behavior. He reports specific behavior patterns which support the assumption that taxpayers try to understand when audits happen and learn when it pays to be honest and when cheating is less risky. The authors term this the "bomb crater" effect since in war, troops under heavy enemy fire would hide in the craters of recent explosions, believing it to be highly unlikely that the next bombs would fall exactly in the same spot in a short time span. Something similar seems to happen in the context of tax audits. }.  In this case taxpayers that recently were audited have their perceived audit rate decrease for a short time period. We have chosen to model the gambler's fallacy effect based on using two separate results published in the literature.  The 2015 National Taxpayers Advocate's annual presort to congress which found that audits indeed have a detrimental long-term impact on the reporting behavior of taxpayers who do not experience an additional tax assessment, and three years after having undergone enforcement activity, these taxpayers report around 35\% less in taxable income than the control group \cite{SebastianBeer2016}. In a different study by Kirchler, Maciejovsky and Schwarzenberger that was based on an a laboratory experiment, the authors considered a 50\% effective tax rate, 50\% sanction rate and a 30\% audit risk and they also found that compliance decreases immediately after an audit \cite{Kirchler2003a}.The decrease was from a 57\% baseline to 33\% one year after the audit. Hence, participates declared 42\% (i.e., $=1-33/57$) less taxable income.   To model the bomb crater effect,  we thus assume that 1 and 3 years after an audit, taxpayers perceived audit rate that is 42 and 35\% below their baseline. We fit a linear relationship until the baseline line audit rate is recovered. Thus percieved audit rate drops to 1-0.42 = 0.58 in year 1 and 1-90.35 = 0.65 in year 3. 



### Old function
# n.power.exp <- n
# if(length(n) > 0)
# {
#   
#   n<-n+1
#   above.t.indices <- which(n>threshold)
#   below.t.indices <- which(n <= threshold)
#   n.power.exp[above.t.indices] <- n[above.t.indices]^Inf
#   n.power.exp[below.t.indices] <- n[below.t.indices]^exponent
# }  
# return((x^(1+1/n.power.exp)))
### Note default exponent:
### p0<- 3*0.008 => exponent <- log(log(p0)/log(1-0.35))/log(3) 


