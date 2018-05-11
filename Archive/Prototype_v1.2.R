remove(list = ls(all = TRUE))
gc()

##Set Seed 
set.seed(55279) ### first seed given by Chris Marcum
set.seed(55333) 

#Setting options
options(warn=1)
options(showErrorCalls=TRUE)
options(showWarnCalls=TRUE)

### Find on which platform and macchine we are running this on
if(.Platform$OS.type == "windows"){
  if (Sys.info()["nodename"]=="" ){
    OSdir <- paste("" ,sep="")}
  memory.limit(4000)
}else{ 
  if (Sys.info()["nodename"]=="vardavas-r.rand.org" | 
      Sys.info()["nodename"]=="vardavas-r.local"){
    OSdir <- paste("/Users/rvardava/Documents/Projects_2015/Tax_Evasion/R code/" ,sep="")}
  else {
    OSdir <- paste("/Users/rvardava/Documents/Projects_2015/Tax_Evasion/R code/" ,sep="")
  }
}

library(ggplot2)
library(Hmisc)
library(igraph)

### set Working Directory
setwd(OSdir)

library.dir   <- "Library/"

library <- file.path(library.dir, "library.R")
source(library)



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
###        Mechanisums Control Options            
#############################################################

penalty.behavioral.effect <- T
network.interactions <- T

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
if(letter.rate < audit.rate) stop("letter.rate < audit.rate")


### at the moment we are not considering letters - since we cleanly split self-employed from 
### salaried tax payers - the latter have no additional income. 
prop.letters.to.audit <- 0.7
audit.rate.w.letter <- prop.letters.to.audit*audit.rate/letter.rate
audit.rate.wout.letter <- (audit.rate-letter.rate*audit.rate.w.letter)/(1-letter.rate)

detection.efficacy <- 0.8 ### 80% detection and 100% discovery: all amount undrreported is found if detected.
penalty.rate <- 0.25
K <-3

#############################################################
###        Media and Tax complexity            
#############################################################

media.feedback <- 0
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


s.discount <- 0.86  ## half life is ~2 years
threshold.time <-  round(2*(-log(2)/log(s.discount)),0)
s.discount.seq.default <- s.discount^seq(0, K, by=1)
s.audit.discount <- s.discount^0.25
s.generation.discount <- exp(-log(2)/40)  # half life is 40 years.
s.penalty.discount <- s.generation.discount

### mu is the value for per.audit.rate*per.penalty.rate when c1.tilde is half way between c1 and c2.   
mu <- 0.015 


c2<-0.7
c1<- rbeta(N, 0.85, 1.1, ncp = 0)*c2##pmin(rexp(N, rate = log(2)/0.3),c2) ### median of the distribution is 0.3
c1<- sort(c1)
compliant.t0 <- rep(TRUE,N)
compliant<- compliant.t0
compliant.history<- compliant.t0

audited.non.compliant <- rep(FALSE,N)
penalized <- rep(FALSE,N)

cV.new<- rep(0,N)
cV.max <- rep(max(cV.new),N)

V.new<- rep(0,N)
V.max <- rep(max(V.new),N)

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

beta.morale <- 0.05
beta.audit  <- beta.morale

ave.degree <- min(5,N/10)
phi <- ave.degree/(N-1)
g <- erdos.renyi.game(N, phi)

## set attributes of the nodes
#g<-set.vertex.attribute(g, i, index=V(g), j)

### get list of neigbours 
### (we do this here and not in the loop since we assume a static network)
nn <- list()
nn <- sapply(tax.ids,FUN=function(i) {neighbors(g, i)})


#############################################################
###        Output objects        
#############################################################


track.dyn <- list()
income.reported <- list()
R0<-R1<-R3<-NULL


#############################################################
###                                                       ###
###        Run Dynamics              
###                                                       ###
#############################################################


for(t in 1:final.year){

  #############################################################
  ###        Update Past year's variables              
  #############################################################
  
  past.year.compliant <- compliant
  past.year.audited.non.compliant <- audited.non.compliant
  past.year.penalized <- penalized
  cV.past <- cV.new
  V.past <- V.new
  Q.past <- Q.new
  P.past <- P.new
  
  #############################################################
  ###        Taxpayers decsions              
  #############################################################
  
  ### Determine how taxpayers view themselves in terms of fully comply or evade
  cV.new <- s.discount*cV.past+
            as.numeric(past.year.compliant | past.year.penalized)
  cV.max <- s.discount*cV.max+1
  
  self.view.compliant <- cV.new/cV.max
  
  
  ### Determine the c1.tilde i.e.,  the max tax rate which taxplay feels un-necessary to risk under-reporting
  c1.tilde <- c1.tilde.fn(c1,c2,tax.rate,per.audit.rate,per.penalty.rate,
                          mu,nu,avg.nu,media.feedback,tax.complexity)
  
  ### Determine the proportion of income taxpayer wishes to report given 
  ### (i) tax rate, (ii) no past experiences and (iii) no network effects
  Delta <-fairness.fn(c1.tilde,c1,c2,tax.rate)
  
  
  ### Set Delta = 0 for not compliant and not penalized in past year.
  ### Thus an evader's proportion of income taxpayer wishes to report\
  ### is influenced by past year success at evading. 
  Delta <- Delta*as.numeric(past.year.compliant | past.year.penalized)
  
  ### Network effects on willingness-to-comply and tax fairness
  ### Note: willingness-to-comply includes audit perceptions and network effects on 
  ## audit perceptions is included below. This could be considered a 
  ## double counting of the effect. I have arguements for otherwise. 
  if(network.interactions){
    Delta<- sapply(tax.ids,FUN=function(i){
      if(length(nn[[i]])>0){
        r <- (1-beta.morale)*Delta[i]+beta.morale* mean(Delta[nn[[i]]])
      }else{
        r<- Delta[i]
      }
      return(r)
    })
  }
  
  V.new <- s.discount*V.past+Delta
  V.max <- s.discount*V.max+1
  
  ### find proportion of income that the taxpayer wishes to report.
  propensity.prop.income.report <- V.new/V.max
  
  ### find compliance probability
  w <- propensity.prop.income.report*self.view.compliant
  #w <- propensity.prop.income.report*as.numeric(years.since.last.compliant<threshold.time) 
  
  
  ### Determine if taxpayers are compliant 
  random.numbers.to.compare <- runif(N,0,1)
  
  compliant <- (w>=random.numbers.to.compare)  #### This needs to be rethought!!!
  ## PROBLEM: I don't like this below but for now its ok. 
  #compliant[years.since.last.compliant>threshold.time] <-FALSE
  
  years.since.last.compliant <- years.since.last.compliant+as.numeric(!compliant)
  years.since.last.compliant[compliant] <- 0
  
  ### Determine how much those non-compliant report
  report <- income
  report[!(compliant)] <- income[!(compliant)]*
                          propensity.prop.income.report[!(compliant)] 
  report<- round(report,0)
  
  ### Update compliance history
  compliant.history <- append.to.history(compliant.history,compliant,K)
  under.reporting.history <- append.to.history(under.reporting.history,
                                               income-report,K)
  
  reporting.history<- income -under.reporting.history
  
  l <- nrow(reporting.history)
  rownames(reporting.history) <- paste("% reported",seq((l-1),0,-1),
                                       "years ago",sep=" ")
  
  
  #############################################################
  ###        Tax authority              
  #############################################################

  
  ### Audit. For the time being we assume random audit 
  audit <- rep(FALSE,N)
  audit <- (runif(N,0,1)<=audit.rate)
  
  audit.history <- append.to.history(audit.history,audit,K)
  freq.audits <- freq.audits+as.numeric(audit)
  
  years.since.last.audit <- years.since.last.audit +1
  years.since.last.audit[audit]<-0
  
  ### Find those that are audited and not compliant. 
  audited.non.compliant <- audit & (!compliant)
  
  penalized <- rep(FALSE,N)
  penalized[audited.non.compliant] <- (runif(sum(audited.non.compliant),0,1)<=
                                         detection.efficacy)
  
  ### Update penalized history.
  penalty.history <- append.to.history(penalty.history,
                                       penalized,K)
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
  ### individual has paid his/her past K penalties.
  
  penalty.history<-correct.past.penalties(penalty.history)
  
  years.since.last.penalty <- years.since.last.penalty+1
  years.since.last.penalty[penalized] <- 0
  
  #############################################################
  ###        Update Risk Perceptions            
  #############################################################
  
  ### Update Percieved Audit Rate
  Q.new <- s.audit.discount*Q.past + as.numeric(audit)
  Q.max <- s.audit.discount*Q.max+1
  Delta.Q <- Q.new/Q.max
  per.audit.rate <- (transformed.per.audit.rate.0+Delta.Q)/
                    (transformed.per.audit.rate.0+1)
  
  ### Update Percieved Penalty Rate
  P.new <- s.penalty.discount*P.past+as.numeric(penalty.rate-per.penalty.rate)*
    as.numeric(penalized)
  per.penalty.rate <- per.penalty.rate.0+P.new
  
  #############################################################
  ###Convert assigned Penalty into a change in compliance behavior             
  #############################################################

  if( penalty.behavioral.effect & any(penalized)){
  num.years.penalty.represents <-penalty.and.past.tax.to.pay/(tax.rate*income)
  
  Delta.Penalty<- num.years.penalty.represents[penalized]
  
  V.new[penalized] <- s.discount*V.new[penalized]+Delta.Penalty
  
  V.max[penalized] <- s.discount*V.max[penalized]+Delta.Penalty
  
  V.new[penalized] <- V.max[penalized]
  cV.new[penalized] <- cV.max[penalized]

  }
  
  #############################################################
  ###      Network (i.e.,Local) level interactions            
  #############################################################
  
  ### PROBLEM:  I think that network effect for tax moral is prabably correctly modeled.
  ### but here we should do something different: beta is the prob of observing
  ### or being communicated of a nn audit event. Once communicated it has
  ### equal impact as if it were experienced by oneself. 
  
  if(network.interactions){
    
    per.audit.rate<- sapply(tax.ids,FUN=function(i){
      if(length(nn[[i]])>0){
        r <-(1-beta.audit)*per.audit.rate[i]+beta.audit* mean(per.audit.rate[nn[[i]]])
      }else{
        r<- per.audit.rate[i]
      }
    })
  }

  #############################################################
  ###    Track Outputs            
  #############################################################
  
  pop0<- (years.since.last.penalty==0)
  pop0<- !compliant
  pop1<- (years.since.last.penalty==1)
  pop3<- (years.since.last.penalty==3)
  
  R0<- c(R0,mean(report[(years.since.last.penalty==0)]))
  R1<- c(R1,mean(report[(years.since.last.penalty==1)]))
  R3<- c(R3,mean(report[(years.since.last.penalty==3)]))
  
   
  track.dyn[[t]] <- data.frame(Delta=Delta,V.new=V.new,w=w,
                      compliant=compliant,report=report,stringsAsFactors = FALSE)
  income.reported[[t]] <- round(w,2)
}


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

#income.reported<- do.call("rbind",income.reported)
#income.reported

#############################################################
###                                                       ###
###       Create Plots             
###                                                       ###
#############################################################
dat <- as.data.frame(t(out))
dat$report<-dat[,"% reported 0 years ago"]
dat$recently.compliant <-as.factor(years.since.last.compliant<threshold.time)
dat$recently.penalized <-as.factor(years.since.last.penalty<threshold.time)
dat$penalized.3.yearsago <-as.factor(years.since.last.penalty==3)
dat$penalized.1.yearsago <-as.factor(years.since.last.penalty==1)

tmp <-dat$years.since.last.penalty
tmp[tmp>3] <- Inf
tmp <- 1/(tmp+1)
tmp<-as.factor(tmp)
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
  geom_point(aes(x=w, y=report, colour = recently.compliant,
                 size=penalty.lag,alpha=c1.tilde))+
  xlab("prob to fully report (w)") +
  ylab("proportion of income reported")+
  ggtitle(paste("proportion of income reported",sep=""))


#############################################################
###                                                       ###
###       Track output            
###                                                       ###
#############################################################

print(out[,1:10])
print(w.plot)
print(report.plot)
print(report.compliance.prob.plot)

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
 
 print(paste("R0=",round(mean(R0[!is.nan(R0)]),2),sep=""))
 print(paste("R1=",round(mean(R1[!is.nan(R1)]),2),sep=""))
 print(paste("R3=",round(mean(R3[!is.nan(R3)]),2),sep=""))


