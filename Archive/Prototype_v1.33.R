remove(list = ls(all = TRUE))
gc()

### Set Working Directory: please change to your working directory 
wk.dir <- "/Users/rvardava/Documents/Projects_2015/Tax_Evasion/R code/"

### Find on which platform and macchine we are running this on
if(.Platform$OS.type == "windows"){
  if (Sys.info()["nodename"]=="" ){
    OSdir <- paste(wk.dir,sep="")}
  memory.limit(4000)
}else{ 
  if (Sys.info()["nodename"]=="vardavas-r.rand.org" | 
      Sys.info()["nodename"]=="vardavas-r.local"){
    OSdir <- paste(wk.dir,sep="")}
  else {
    OSdir <- paste(wk.dir,sep="")
  }
}
### set Working Directory
setwd(OSdir)

### Load Libraries: please ensure you have installed these R packages.
library(ggplot2)
library(Hmisc)
library(igraph)
library(reshape2)

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

library.dir   <- "Library/"

library <- file.path(library.dir, "library.R")
source(library)

##Set Seed 
set.seed(55279) ### first seed given by Chris Marcum
set.seed(55333) 

#############################################################
###                                                       ###
###        Set Directories and Input/Output file names              
###                                                       ###
#############################################################



#############################################################
###                                                       ###
###        Settings              
###                                                       ###
#############################################################

#############################################################
###        Mechanisms Control Options            
#############################################################

### Allow for both tax moral and risk of audit perceptions to spread over the network 
network.interactions <- F

### As opposed to strong tendency to full evasion in a mild.tenency.to.full.evasion 
### those that under-report and are not penalized do not immediately tend towards
### full evasion (i.e., Delta =0) rather stochastically based on what they reported 
### in previous years will either tend to what they consider is fair or towards full evasion.
mild.tendency.to.full.evasion <- TRUE

### With a tax.refund.effect those that recieved a refund return are roughly nearly
### half as likely to initiate in under-reporting.
tax.refund.effect <- TRUE

### Endogenous or Targetted auditing assumes that the tax collecting agency will identify
### returns to audit based on comparision to past tax returns
target.auditing <- TRUE

### Include the effect of a penalty for underreporting. Those that under-report are
### likely to become compliant based on how much their cumulative penalty (considering back-audits)
### measures relative to the expected annual amount of taxes they owe if they hadn't underreported
penalty.behavioral.effect <- TRUE

### Include the gamblers fallacy: 
### it is expected that the more time that passes without audits, the
### higher the perceived probability of an audit might be. 
### This is likely to affect just those that underreport for a short time window.
gambelers.fallacy <- TRUE

### Generally, it was found that, after an audit, evasion remained high and then decreased.
### This may be due to the bomb.crater effect whereby individuals underestimate the risk
### of being audited in two separate occasion within a small time-window
bomb.crater <- TRUE 

### Media Effect
media.effect <- TRUE


#############################################################
###        Population Size and Number of Years             
#############################################################

### set population size
N<- 1000
tax.ids <- 1:N

### number of years to run
final.year<- 100

#############################################################
###        Government Controls              
#############################################################
tax.rate<- 0.3
nu <- 0
avg.nu <- 0.3

#############################################################
###        Tax Authority Controls              
#############################################################

letter.rate <- 0.05
audit.rate<- 0.01
#if(letter.rate < audit.rate) stop("letter.rate < audit.rate")


### at the moment we are not considering letters - since we cleanly split self-employed from 
### salaried tax payers - the latter have no additional income. 
#prop.letters.to.audit <- 0.7
#audit.rate.w.letter <- prop.letters.to.audit*audit.rate/letter.rate
#audit.rate.wout.letter <- (audit.rate-letter.rate*audit.rate.w.letter)/(1-letter.rate)

detection.efficacy <- 0.8 ### 80% detection and 100% discovery: all amount undereported is found if detected.
penalty.rate <- 0.25
K <- 3

#############################################################
###        Media and Tax complexity            
#############################################################

media.feedback <- 0
media.mid.effect <- 0.40 ### at 40% tax gap media effect is 0.5
media.steepness <- 6    ### steepness of the media effect: its invers is a log normal stand. dev. 

tax.complexity <- 0

#############################################################
###        Initial Risk Perceptions             
#############################################################

per.audit.rate.0 <- rep(audit.rate,N)
per.audit.rate<- per.audit.rate.0
transformed.per.audit.rate.0 <- per.audit.rate.0/(1-per.audit.rate.0)


per.penalty.rate.0 <- rep(penalty.rate-0.15,N)
transformed.per.penalty.rate.0 <- per.penalty.rate.0/(1-per.penalty.rate.0)
per.penalty.rate <- per.penalty.rate.0

impact.letter.wrt.audit <- 0.25


#############################################################
###        Tax-payers attributes           
#############################################################


s.discount <- exp(-log(2)/2) ## half life is ~2 years
threshold.time <-  round(2*(-log(2)/log(s.discount)),0)
s.discount.seq.default <- s.discount^seq(0, K, by=1)
s.audit.discount <- s.discount
s.generation.discount <- exp(-log(2)/40)  # half life is 40 years.
s.penalty.discount <- s.generation.discount

### mu is the value for per.audit.rate*per.penalty.rate when c1.tilde is half way between c1 and c2. 
### E.g. if per.audit.rate=10% and per.penalty.rate =25% and mu =0.025 - then assuming that all else 
### stay set at default values (e.g., media etc.)  c1.tilde will be midway between c1 and c2. 
mu <- 0.0025 
penalty.asymmetery.factor <- 1  ## based on \cite{Friedland1978}  we could range this between 1 and 1.2


return.weight <- 1-32.6/58 ### see Christian 1994

c2<-0.7
c1<- rbeta(N, 0.85, 1.1, ncp = 0)*c2 ### median of the distribution is 0.3
c1<- sort(c1)
compliant.t0 <- rep(TRUE,N)
compliant<- compliant.t0
compliant.history<- compliant.t0

audited.non.compliant <- rep(FALSE,N)
penalized <- rep(FALSE,N)

V.new<- rep(0,N)
V.max <- rep(max(V.new),N)

Delta <- rep(1,N)
propensity.prop.income.report<- rep(1,N)

refund.return <- rep(TRUE,N)


Q.new<- rep(0,N)
Q.max <- max(Q.new)

P.new<- rep(0,N)
P.max <- max(P.new)

income<- rep(100,N)
under.reporting.history<- rep(0,N)
penalty.history<- rep(FALSE,N)
audit.history<- rep(FALSE,N)

years.since.last.compliant <- rep(0,N)
years.since.last.audit <- rep(0,N)
years.since.last.penalty <- rep(0,N)

freq.audits <- rep(0,N)
freq.penalty <- rep(0,N)

#############################################################
###        Create the Network          
#############################################################

beta.morale <- 0.1
beta.audit  <- beta.morale
yearly.prop.interactions <- 0.2

ave.degree <- 5
phi <- ave.degree/(N-1)
g <- erdos.renyi.game(N, phi)

## set attributes of the nodes
#g<-set.vertex.attribute(g, i, index=V(g), j)

### get list of neigbours 
### (we do this here and not in the loop since we assume a static network)
nn <- list()
nn <- sapply(tax.ids,FUN=function(i) {neighbors(g, i)})
nn.int <- nn


#############################################################
###        Output objects        
#############################################################


track.dyn <- list()
aggregated.dyn <- list()
R0<-R1<-R3<-Rannoyed<- NULL


#############################################################
###                                                       ###
###        Run Dynamics              
###                                                       ###
#############################################################


for(t in 1:final.year){
  
  #############################################################
  ###        Model a simple Markov Process for refund return              
  #############################################################
  
  additional.to.refund <- get.additional.to.refund(refund.return,prob.additional.to.refund=0.1)
  refund.return[additional.to.refund] <- TRUE
  
  refund.to.additional <- get.refund.to.additional(refund.return,prob.refund.to.additional=0.05)
  refund.return[refund.to.additional] <- FALSE
  

  #############################################################
  ###        Update Past year's variables              
  #############################################################
  
  past.year.compliant <- compliant
  past.year.audited.non.compliant <- audited.non.compliant
  past.year.penalized <- penalized
  V.past <- V.new
  Q.past <- Q.new
  P.past <- P.new
  
  ### Determine the subset of nn with whom they interact with that year. 
  if(network.interactions){
    nn.int<-get.nn.int(tax.ids,nn,yearly.prop.interactions=yearly.prop.interactions)
    prop.of.those.int<-get.interacting.proportion.of.nn(tax.ids,nn,nn.int)
  }
  
  #############################################################
  ###        Taxpayers decsions              
  #############################################################
  
  
  ### Determine the c1.tilde i.e.,  the max tax rate which taxpayer feels un-necessary to risk under-reporting
  c1.tilde <- c1.tilde.fn(c1,c2,tax.rate,per.audit.rate,per.penalty.rate*penalty.asymmetery.factor,
                          mu,nu,avg.nu,media.feedback,tax.complexity)
  
  ### Treat those that were compliant or penalized last year differently from those
  ### non-compliant and non-detected. 
  compliant.or.penalized <- past.year.compliant | past.year.penalized
  non.compliant.non.penalized <- !compliant.or.penalized
  
  
  ### Delta represents a taxpayers proportion of income he wishes to report
  ### considering the factors such as his/her tax rate, perceieved deterence, 
  ### media, tax complexity and percieved return on services. 
  
  ### All compliant individuals and those just penalized based their Delta on the fairness fn.
  ### For these tax payers, Delta does NOT include past experiences effects.
  if(any(compliant.or.penalized)){
    Delta[compliant.or.penalized] <- 
      fairness.fn(c1.tilde[compliant.or.penalized],c1,c2,tax.rate)
    
    if(tax.refund.effect){
      ### In addition to tax moral we now add the effect of receiving a refund return or 
      ### or additional payment.
      tmp.pop <- (refund.return & compliant.or.penalized)  
      Delta[tmp.pop] <- (1-return.weight)*Delta[tmp.pop]+ return.weight*1
      tmp.pop <- (!refund.return & compliant.or.penalized) 
      Delta[tmp.pop] <- (1-return.weight)*Delta[tmp.pop]
    }
  
  }
  
  ### Some of those non compliant and non penalized in the past year also based their Delta on
  ### the fairness fn but others will be encoraged by their recent success in evading and will
  ### have a Delta =0. (i.e., they tend to zero reporting)
  ### note that there is a positive feedback here: Delta=0 is more likely if you evaded and not caught.
  if(any(non.compliant.non.penalized)){
    Delta[non.compliant.non.penalized] <- 0 
    
    if(mild.tendency.to.full.evasion){
      ### however the tendency of under-reporting for some of these individuals may not be towards
      ### full evasion
      w.non<-propensity.prop.income.report
      w.non[compliant.or.penalized] <- NA
      random.numbers.to.compare <- runif(length(w.non),0,1)
      partial <- (w.non>=random.numbers.to.compare) 
      partial <- non.compliant.non.penalized & partial
      if(any(partial)){
      Delta[partial] <- fairness.fn(c1.tilde[partial],c1,c2,tax.rate)
      }
    }
  }
  
  ### Delta is finally influenced by Social Network contacts 
  if(network.interactions){
    Delta<- do.network.interactions(Delta,beta.morale,nn.int,prop.of.those.int)
  }
  
  ### Find proportion of income that the taxpayer wishes to report by weighing Delta
  ### against past experiences. 
  ### First step: Tally up the experiences with tax compliance and discount past experiences wrt present.
  V.new <- s.discount*V.past+Delta
  ### Second step: Compute the maximum value that the Tally could have been if all past experiences 
  ### complying were positive. 
  V.max <- s.discount*V.max+1
  ### Third step: Find proportion of income that the taxpayer wishes to report 
  propensity.prop.income.report <- round(V.new/V.max,2)
  propensity.prop.income.report[propensity.prop.income.report<0.10] <- 0
  #propensity.prop.income.report[propensity.prop.income.report>=0.95] <- 1
  
  ### Determine if taxpayers are fully compliant this year: those that were not compliant last year
  ### nor penalized last year will be fully compliant unless their propensity.prop.income.report is 100%
  compliant[non.compliant.non.penalized] <- FALSE
  compliant[propensity.prop.income.report==1] <- TRUE
  
  ### Those who were compliant or have been penalized last year are susceptible to initiate evasion.
  suseptibles.to.underreport.pop  <- (compliant.or.penalized & propensity.prop.income.report<1)
  random.numbers.to.compare <- runif(sum(suseptibles.to.underreport.pop),0,1)
  initiation.prob <- propensity.prop.income.report[suseptibles.to.underreport.pop]
  ### Determine which of the susceptible taxpayers initiate evasion. 
  compliant[suseptibles.to.underreport.pop] <- (initiation.prob>=random.numbers.to.compare) 
  
  ### Interpret these as probabilities to fully-comply:
  w <- propensity.prop.income.report
  w[non.compliant.non.penalized] <- 0
  w[propensity.prop.income.report==1] <- 1
  w[suseptibles.to.underreport.pop] <- initiation.prob
  
  ### track number of years since last fully compliant.
  years.since.last.compliant <- years.since.last.compliant+as.numeric(!compliant)
  years.since.last.compliant[compliant] <- 0
  
  ### Determine the amount reported by those fully compliant 
  report <- income
  ### Determine the amount reported by those non-compliant 
  report[!(compliant)] <- income[!(compliant)]*
                          propensity.prop.income.report[!(compliant)] 
  report<- round(report,0)
  
  ### Update compliance history
  compliant.history <- append.to.history(compliant.history,
                                         compliant,K,label="compliant")
  under.reporting.history <- append.to.history(under.reporting.history,
                                               income-report,K,label="% reported")
  
  reporting.history<- income -under.reporting.history
  
  
  #############################################################
  ###        Tax authority              
  #############################################################

  
  audit <- rep(FALSE,N)
  
  if(target.auditing){
    ### Target Audit. Assume we know those more likely to evade.
    target <- (Delta==0)
    target.audit<- (runif(sum(target),0,1)<= audit.rate*N/sum(target))
    audit[target]<- target.audit  
  }else{
    ### Audit. For the time being we assume random audit
    audit <- (runif(N,0,1)<=audit.rate)
  }

  
  ### update that number of audits each tax payer experienced.
  audit.history <- append.to.history(audit.history,audit,K,label="audited")
  freq.audits <- freq.audits+as.numeric(audit)
  
  years.since.last.audit <- years.since.last.audit+1
  years.since.last.audit[audit]<-0
  
  ### Find those that are audited and not compliant. 
  audited.non.compliant <- audit & (!compliant)
  
  ### Find those that are penalized from those audited and not compliant.
  penalized <- rep(FALSE,N)
  penalized[audited.non.compliant] <- (runif(sum(audited.non.compliant),0,1)<=
                                         detection.efficacy)
  
  ### Update penalized history.
  penalty.history <- append.to.history(penalty.history,penalized,K,label="penalty")
  freq.penalty<- freq.penalty+as.numeric(penalized)
  
  ### Find the Penatly to pay. 
  ### Note the multiplication by (!penalty.history). This ensures
  ### those audited and penalized in K previous years - aren't penalized again for
  ### the same under-reporting year. 
  sum.non.penalized.past.under.reporting <- colSums(under.reporting.history*
                                                      (!penalty.history))
  
  past.tax.to.pay <- as.numeric(penalized)*(tax.rate)*
                     sum.non.penalized.past.under.reporting  
  
  penalty.to.pay <- as.numeric(penalized)*(penalty.rate)*
                    sum.non.penalized.past.under.reporting
  
  penalty.and.past.tax.to.pay <- past.tax.to.pay + penalty.to.pay
                    
  
  
  ### Correct penalized history by recording that an audited and penalized 
  ### individual has paid his/her past K years of penalties.
  
  penalty.history<-correct.past.penalties(penalty.history)
  
  years.since.last.penalty <- years.since.last.penalty+1
  years.since.last.penalty[penalized] <- 0
  
  #############################################################
  ###        Update Risk Perceptions            
  #############################################################
  
  ### Update Percieved Audit Rate
  if(network.interactions){
    Delta.Q <- do.network.interactions(as.numeric(audit),1,
                          nn.int,prop.of.those.int,method="max")
  }
  else{
    Delta.Q <- as.numeric(audit)
  }
  Q.new <- s.audit.discount*Q.past + Delta.Q
  Q.max <- s.audit.discount*Q.max+1
  w.Q <- Q.new/Q.max
  per.audit.rate <- (transformed.per.audit.rate.0+w.Q)/
                    (transformed.per.audit.rate.0+1)
  
  if(gambelers.fallacy){
    ### Find those that are not audited and not compliant. 
    non.audited.non.compliant <- (!audit) & (!compliant)
    ### Induce cold feet effect on these individuals - 
    ### by increasing their audit risk perception
    per.audit.rate[non.audited.non.compliant] <- 
      get.per.audit.gamblers.fallacy(per.audit.rate[non.audited.non.compliant],
                       years.since.last.audit[non.audited.non.compliant])
  }
  
  if(bomb.crater){
    ### Find those that are audited and not penalized. 
    audited.non.penalized <- audit & (!penalized) 
    ### Reduce percieved risk of getting audited again.
    per.audit.rate[audited.non.penalized] <- 
      get.per.audit.bomb.crater(per.audit.rate[audited.non.penalized],
                        years.since.last.audit[audited.non.penalized])
  }
  
  
  ### Update Percieved Penalty Rate
  P.new <- s.penalty.discount*P.past+as.numeric(penalty.rate-per.penalty.rate)*
    as.numeric(audit) ### even if not penalized but audited you get a better sense of penaty rate.
  per.penalty.rate <- per.penalty.rate.0+P.new
  
  #############################################################
  ### Convert assigned Penalty into a change in compliance behavior             
  #############################################################

  if( penalty.behavioral.effect & any(penalized)){
    
    ### Express the total penalty as a fraction of the expected tax revenue owed. 
    #num.years.penalty.represents <- penalty.to.pay/(tax.rate*income)
    num.years.penalty.represents <- penalty.and.past.tax.to.pay/(tax.rate*income)
    
    Delta.Penalty <- num.years.penalty.represents[penalized]
    
    V.new[penalized] <- s.discount*V.new[penalized]+Delta.Penalty
    
    V.max[penalized] <- s.discount*V.max[penalized]+Delta.Penalty
  }
  
  
  #############################################################
  ###   Media             
  #############################################################
  
  ### Compute the tax gap 
  expected.tax.revenue<- tax.rate*sum(income)
  tax.revenue <- tax.rate*sum(report)
  tax.gap.amount <- expected.tax.revenue- tax.revenue
  tax.gap <- tax.gap.amount/expected.tax.revenue
  ### EU tax gap see http://www.socialistsanddemocrats.eu/sites/default/files/120229_richard_murphy_eu_tax_gap_en.pdf
  ###US tax gap see https://www.irs.gov/pub/newsroom/overview_tax_gap_2006.pdf
  ### US tax  gap is 14%, Italy & Greece 27% and Sweeden, Denamrk, Germany and UK are 
  ### 18.8,17.7,16.0 and 12.5% respectively. 
  
  if(media.effect){
    media.feedback <- (1-tax.gap)*cdf.lognormal(x=tax.gap, log(media.mid.effect),1/media.steepness)
  }
  
  #############################################################
  ###    Track Outputs            
  #############################################################
  
  if (nrow(audit.history)==4){ 
    ### The annoyed popultion are those audited that are not found to underreport 
    ### and thus were not penalized. 
    pop.annoyed <- (audit.history["audited 3 years ago",] & 
                      !(penalty.history["penalty 3 years ago",]) &
                      reporting.history["% reported 3 years ago",]>0)
    tmp.reported.0.years.ago <- report[pop.annoyed]
    tmp.reported.3.years.ago <- reporting.history["% reported 3 years ago",pop.annoyed]
    
    Rannoyed <- c(Rannoyed,mean(1-(tmp.reported.0.years.ago/tmp.reported.3.years.ago)))
  }
  
  ### The pop0 population is the population penalized in the current year.
  ### pop1 and pop3 those penalized 1 and 3 years ago. 
  pop0<- (years.since.last.penalty==0)
  pop1<- (years.since.last.penalty==1)
  pop3<- (years.since.last.penalty==3)

  
  R0<- c(R0,mean(report[pop0]))
  R1<- c(R1,mean(report[pop1]))
  R3<- c(R3,mean(report[pop3]))
  
   
  track.dyn[[t]] <- data.frame(t=t,
                               tax.ids=tax.ids,
                               per.audit.rate=round(per.audit.rate,2),
                               per.penalty.rate=round(per.penalty.rate,2),
                               c1.tilde=round(c1.tilde,2),
                               Delta=round(Delta,2),
                               V.new=round(V.new,2),
                               w=round(w,2),
                               compliant=compliant,
                               report=report,
                               audit=audit,
                               penalized=penalized,
                               stringsAsFactors = FALSE)
  
  aggregated.dyn[[t]] <- c(t=t,
                           tax.gap=tax.gap,
                           media.feedback=media.feedback,
                           mean.per.audit.rate=mean(per.audit.rate),
                           sd.per.audit.rate=sd(per.audit.rate),
                           mean.per.penalty.rate = mean(per.penalty.rate),
                           sd.per.penalty.rate = sd(per.penalty.rate))
  
}


#############################################################
###                                                       ###
###       Arrange Output Data           
###                                                       ###
#############################################################

track.id <- do.call("rbind",track.dyn)
track.id.list <- split(track.id,track.id$tax.ids)

aggregated.dyn <- as.data.frame(do.call("rbind",aggregated.dyn))

melt.aggregated.dyn <- melt(aggregated.dyn,id.vars = "t",variable.name = "agg_variable", 
                            value.name = "agg_value")


out <- rbind(c1=round(100*c1,0),
             c1.tilde=round(100*c1.tilde,0),
             per.audit.rate=round(100*per.audit.rate,0),
             per.penalty.rate=round(100*per.penalty.rate,0),
             Delta = round(100*Delta,0),
             w = round(100*w,0),
             freq.audits=freq.audits,
             freq.penalty=freq.penalty,
             years.since.last.compliant =years.since.last.compliant,
             years.since.last.audit=years.since.last.audit,
             years.since.last.penalty=years.since.last.penalty,
             reporting.history)


select.trajectories <- 3
tmp1<-intersect(track.id$tax.id[track.id$t%in%50 & track.id$report>95], 
               track.id$tax.id[track.id$t%in%100 & track.id$report<5] )
if(length(tmp1)>select.trajectories) tmp1<- sample(tmp1,select.trajectories)

focus.ids<- tmp1

tmp2<-intersect(track.id$tax.id[track.id$t%in%50 & track.id$report<50], 
               track.id$tax.id[track.id$t%in%100 & track.id$report>90] )
if(length(tmp2)>select.trajectories) tmp2<- sample(tmp2,select.trajectories)

tmp3<- sample(1:N,select.trajectories)
  

focus.ids <- c(tmp1,tmp2,tmp3)

track.id.focus.ids <- track.id[track.id$tax.ids%in%focus.ids, ]
track.id.focus.ids$tax.ids <- as.factor(track.id.focus.ids$tax.ids)

tmp <-cbind(track.id.focus.ids[track.id.focus.ids$audit, c("t","tax.ids")],type="audit")
tmp <- rbind(tmp,
             cbind(track.id.focus.ids[track.id.focus.ids$penalized, c("t","tax.ids")],type="penalized"))

track.id.focus.ids.penalized <- tmp




#############################################################
###                                                       ###
###       Create Plots             
###                                                       ###
#############################################################
dat <- as.data.frame(t(out))
dat$report<-dat[,"% reported 0 years ago"]
dat$compliant.in.past.3.years <-as.factor(years.since.last.compliant<4)
dat$recently.penalized <-as.factor(years.since.last.penalty<4)
dat$penalized.3.yearsago <-as.factor(years.since.last.penalty==3)
dat$penalized.1.yearsago <-as.factor(years.since.last.penalty==1)

tmp <-dat$years.since.last.penalty
tmp[tmp>3] <- Inf
tmp <- 1/(tmp+1)
tmp<-as.factor(tmp)
levels(tmp) <- c("not penalized with past 3 years","penalized 3 years ago","penalized 2 years ago",
                 "penalized a years ago","penalized this year")
dat$penalty.lag <- tmp


c1.plot <- ggplot(dat) +
  geom_histogram(aes(x=c1,y=..density..),alpha=0.55, fill="pink",,color="pink",binwidth = 2)+
  geom_histogram(aes(x=c1.tilde,y=..density..),alpha=0.55, fill="lightblue",color="blue",binwidth = 2)+
  geom_density(aes(x=c1, y = ..density..),alpha=0.7, fill="pink",color="red",size=0.1) +
  geom_density(aes(x=c1.tilde, y = ..density..),alpha=0.7, fill="lightblue",color="blue",size=0.1)+
  theme_bw() +
  xlab("tax rate") +
  ylab("density")+
    ggtitle(paste("distribution of c1 (red) and c1.tilde (blue)",sep=""))  

w.plot <- ggplot(dat) +
  geom_histogram(aes(x=w),alpha=1, fill="red",binwidth = 2) +
  theme_bw() +
  xlab("propensity to fully report as a %") +
  ylab("count")+
  ggtitle(paste("Histogram of propensity to fully report",sep=""))  

report.plot <- ggplot(dat) +
  geom_histogram(aes(x=report),alpha=1, fill="blue",binwidth = 2) +
  theme_bw() +
  xlab(" % of income reported") +
  ylab("count")+
  ggtitle(paste("Histogram of % of income reported",sep=""))

per.audit.rate.plot <- ggplot(dat) +
  geom_histogram(aes(x=per.audit.rate),alpha=1, fill="red",binwidth = 1) +
  theme_bw() +
  xlab("per.audit.rate as a %") +
  ylab("count")+
  ggtitle(paste("Histogram of the per.audit.rate",sep="")) 

per.penalty.rate.plot <- ggplot(dat) +
  geom_histogram(aes(x=per.penalty.rate),alpha=1, fill="red",binwidth = 1) +
  theme_bw() +
  xlab("per.penalty.rate as a %") +
  ylab("count")+
  ggtitle(paste("Histogram of the per.penalty.rate",sep="")) 

years.since.last.compliant.plot <- ggplot(dat) +
  geom_histogram(aes(x=years.since.last.compliant),alpha=1, fill="red",binwidth = 1) +
  theme_bw() +
  xlab("years.since.last.compliant") +
  ylab("count")+
  ggtitle(paste("Histogram years.since.last.compliant",sep="")) 

report.compliance.prob.plot <- ggplot(dat) +
  geom_point(aes(x=w, y=report, colour = compliant.in.past.3.years,
                 size=penalty.lag,alpha=c1.tilde))+
  xlab("prob to fully report (w) this year") +
  ylab("proportion of income reported this year")+
  ggtitle(paste("proportion of income reported",sep=""))

sample.report.trajectory.plot<- ggplot(track.id.focus.ids) +
  geom_line(aes(x=t, y=report, group= tax.ids, colour = tax.ids),size=1.5)+
  geom_vline(data =track.id.focus.ids.penalized, aes(xintercept = t, colour= type, linetype=type)) +
  facet_wrap(~ tax.ids)+
  xlab("year") +
  ylab("report")+
  ggtitle(paste("proportion of income reported for various tax payers of interest",sep=""))


tax.gap.dyn <- aggregated.dyn[,c("t","tax.gap")]
tax.gap.dyn.lag <- cbind(tax.gap.dyn, tax.gap.lag=c(0,tax.gap.dyn$tax.gap[-nrow(tax.gap.dyn)]) )
tax.gap.dyn.lag <- cbind(tax.gap.dyn.lag, tax.gap.lag2=c(0,tax.gap.dyn.lag$tax.gap.lag[-nrow(tax.gap.dyn.lag)]) )
last.20.years <- nrow(tax.gap.dyn.lag)
last.20.years <-c((last.20.years-75):last.20.years)

iterative.map.plot <- ggplot(tax.gap.dyn.lag[last.20.years,]) +
  geom_point(aes(x=tax.gap.lag, y=tax.gap, color=t),size=3)+
  geom_line(aes(x=tax.gap.lag, y=tax.gap.lag),alpha=0.4,size=2)+
  geom_segment(aes(x = tax.gap.lag2, y = tax.gap.lag, xend = tax.gap.lag, yend = tax.gap,color=t),
               arrow = arrow(length = unit(0.02, "npc"),type = "closed",angle=20))+
  xlab("tax gap in year t") +
  ylab("tax gap in year t+1")+
  ggtitle(paste("Tax Gap Trajectory",sep=""))

aggregated.dyn.plot <- ggplot(melt.aggregated.dyn)+
  geom_line(aes(x=t, y=agg_value, group= agg_variable, colour = agg_variable),size=1.5)+
  #facet_wrap(~ agg_variable)+
  xlab("year")+
  ylab("proportion")+
  ggtitle(paste("Dynamics of Aggregates",sep=""))


#############################################################
###                                                       ###
###       Track output            
###                                                       ###
#############################################################

print(out[,1:10])
print(c1.plot)
print(w.plot)
print(report.plot)
print(per.audit.rate.plot)
print(years.since.last.compliant.plot)
print(report.compliance.prob.plot)
print(sample.report.trajectory.plot)
print(iterative.map.plot)
print(aggregated.dyn.plot)

# pop0<- (dat$years.since.last.penalty==0)
# pop1<- (dat$years.since.last.penalty==1)
# pop3<- (dat$years.since.last.penalty==3)
# 
# R0<- dat$report[pop0]
# R1<- dat$report[pop1]
# R3<- dat$c1[pop3]
# 

 R0<- rev(R0)[1:(final.year/2)]
 R1<- rev(R1)[1:(final.year/2)]
 R3<- rev(R3)[1:(final.year/2)]
 Rannoyed <- rev(Rannoyed)[1:(final.year/2)]
 
 print(paste("R0=",round(mean(R0[!is.nan(R0)]),2),sep=""))
 print(paste("target R0=",0.286*round(mean(R1[!is.nan(R1)]),2),sep=""))
 print(paste("R1=",round(mean(R1[!is.nan(R1)]),2),sep=""))
 print(paste("R3=",round(mean(R3[!is.nan(R3)]),2),sep=""))
 print(paste("target R3=",0.63*round(mean(R1[!is.nan(R1)]),2),sep=""))
 print(paste("Rannoyed=",100*round(mean(Rannoyed[!is.nan(Rannoyed)]),2),"% decrease in reported income",sep=""))
 print(paste("target Rannoyed=",35,"% decrease in reported income",sep=""))
 
 see.nn.info.at.time.t(track.id.list,i=626,nn,64)
 


