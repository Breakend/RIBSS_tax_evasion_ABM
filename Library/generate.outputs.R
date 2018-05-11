#Generates output for the simulation

generate.outputs <- function(config, aggregated.dyn, sim.data, track.dyn, g.info, last.t.yrs=40, cl=NULL)
{
  #print("Generating outputs... ")
  
  network.model <- get.config.param(config, 'network.model')
  network.effect <- ifelse(network.model != FALSE, TRUE, FALSE)
  g <- g.info$g
  nn <- g.info$nn
  
  final.year <- dim(aggregated.dyn)[1] #Final Year
  year <- final.year-last.t.yrs
  track.data <- track.dyn[track.dyn$t > year, ]
  
  #Converting proportions to percentage
  track.data$per.audit.rate <-  track.data$per.audit.rate*100
  track.data$per.audit.rate <- ifelse(track.data$per.audit.rate > 100, 100, track.data$per.audit.rate)
  track.data$per.penalty.rate <- track.data$per.penalty.rate*100
  
  #Calculate the gradient and R-squared for the tax gap in the laast 20 years
  tax.gap.fit <- with(aggregated.dyn[aggregated.dyn$t>year, ], lm(tax.gap~t))
  tax.gap.gradient <- tax.gap.fit$coefficients[2]
  sum.ob <- summary(tax.gap.fit)
  tax.gap.r.sqrd <- sum.ob$r.squared
  tax.gap.percent <- aggregated.dyn$tax.gap[final.year]*100.00
  
  #For swedish data the percent reported is 100.0016 for a couple of observations, hence capping to 100
  track.data$percent.reported.0.years.ago <- ifelse(track.data$percent.reported.0.years.ago > 100.00, 100.00, track.data$percent.reported.0.years.ago)

  prop.income.reported.qs <- get.quantiles(var=track.data$percent.reported.0.years.ago, name.prefix = "prop.income.reported.q")
  
  perc.hideable.income.reported <- track.data$percent.hideable.reported.0.years.ago #100 * (sim.data$percent.reported.0.years.ago - (100 - sim.data$perc.hideable.income))/sim.data$perc.hideable.income
  perc.hideable.income.reported <- ifelse(perc.hideable.income.reported < 0, 0, perc.hideable.income.reported)
  perc.hideable.income.reported.qs <- get.quantiles(var=perc.hideable.income.reported, name.prefix = "perc.hideable.income.reported.q")
  
  per.audit.rate.qs <- get.quantiles(track.data$per.audit.rate, name.prefix = "per.audit.rate.q",
                                     breakpoints = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, Inf))
  per.penalty.rate.qs <- get.quantiles(track.data$per.penalty.rate, name.prefix = "per.penalty.rate.q", 
                                       #breakpoints = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, round(max(sim.data$per.penalty.rate))))
                                       breakpoints = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, Inf))
  mean.prop.income.reported <- mean(track.data$percent.reported.0.years.ago)
  mean.perc.hideable.income.reported <- mean(perc.hideable.income.reported)
  mean.per.audit.rate <- mean(track.data$per.audit.rate)
  mean.per.penalty.rate <- mean(track.data$per.penalty.rate)
  
  sd.prop.income.reported <- sd(track.data$percent.reported.0.years.ago)
  sd.perc.hideable.income.reported <- sd(perc.hideable.income.reported)
  sd.per.audit.rate <- sd(track.data$per.audit.rate)
  sd.per.penalty.rate <- sd(track.data$per.penalty.rate)
  
  pers.stats <- get.persistence.statistic(track.data = track.data, last.t.years = last.t.yrs)
  persistently.compliant <- pers.stats$persistently.compliant
  persistently.non.compliant <- pers.stats$persistently.non.compliant
  total.persistence <- pers.stats$total.persistence

  #Network effects
  if(network.effect)
  {
    vars <- c("report", "hideable.reported", "per.audit.rate", "audited")
    functions <- c(mean, mean, mean, sum)
    track.data <- track.dyn[track.dyn$t>year-1 & track.dyn$t<final.year, ]
    #track.data[, 'audit'] <- track.data[, 'audited']
    alters.mean.effects <- get.nearest.neighbours.effects(track.data, nn, vars, functions, cl=cl)
    colnames(alters.mean.effects) <- vars
    
    #Effect of nearest neighbours' reporting on ego's reporting
    alters.mean.report.t.minus.1 <- alters.mean.effects[, "report"] #track.dyn[track.dyn$t>year-1 & track.dyn$t<final.year, 'alters.mean.report']
    mean.report.fit <- with(track.dyn[track.dyn$t>year, ], lm(report~alters.mean.report.t.minus.1))
    netw.effect.report.gradient <- mean.report.fit$coefficients[2]
    netw.effect.report.r.sqrd <- summary(mean.report.fit)$r.squared
    
    #Effect of nearest neighbours' reporting on ego's hideable income reporting
    alters.mean.hideable.reported.t.minus.1 <- alters.mean.effects[, "hideable.reported"]
    mean.hideable.reported.fit <- with(track.dyn[track.dyn$t>year, ], lm(report~alters.mean.hideable.reported.t.minus.1))
    netw.effect.hideable.reported.gradient <- mean.hideable.reported.fit$coefficients[2]
    netw.effect.hideable.reported.r.sqrd <- summary(mean.hideable.reported.fit)$r.squared
    
    #Effect of nearest neighours's perceived audit rate on ego's perceived audit rate
    alters.mean.par.t.minus.1 <- alters.mean.effects[, "per.audit.rate"] #track.dyn[track.dyn$t>year-1 & track.dyn$t<final.year, 'alters.mean.per.audit.rate']
    mean.per.audit.rate.fit <- with(track.dyn[track.dyn$t>year, ], lm(per.audit.rate~alters.mean.par.t.minus.1))
    netw.effect.per.audit.rate.gradient <- mean.per.audit.rate.fit$coefficients[2]
    netw.effect.per.audit.rate.r.sqrd <- summary(mean.per.audit.rate.fit)$r.squared
    
    #Effect of number of alters audited on ego's compliance status
    alters.audit.count.t.minus.1 <- alters.mean.effects[, "audited"] #track.dyn[track.dyn$t>year-1 & track.dyn$t<final.year, 'alters.audit.count']
    n.alters.audited.fit <- with(track.dyn[track.dyn$t>year, ], lm(compliant~alters.audit.count.t.minus.1))
    netw.effect.compliance.gradient <- n.alters.audited.fit$coefficients[2]
    netw.effect.compliance.r.sqrd <- summary(n.alters.audited.fit)$r.squared
    
    asrt.means <- aggregated.dyn %>% filter(t > year) %>% 
      summarise(mean.nn.over.nc = mean(nn.over.nc),
                mean.cc.over.cn = mean(cc.over.cn), 
                mean.assortativity = mean(assortativity))
    
  } else
  {
    netw.effect.report.gradient <- NA
    netw.effect.report.r.sqrd <- NA
    netw.effect.hideable.reported.gradient <- NA
    netw.effect.hideable.reported.r.sqrd <- NA
    netw.effect.per.audit.rate.gradient <- NA
    netw.effect.per.audit.rate.r.sqrd <- NA
    netw.effect.compliance.gradient <- NA
    netw.effect.compliance.r.sqrd <- NA
    asrt.means <- c("mean.nn.over.nc" = NA, "mean.cc.over.cn" = NA, "mean.assortativity" = NA)
  }
  
     
  return(cbind(tax.gap.percent, tax.gap.gradient, tax.gap.r.sqrd, 
               t(prop.income.reported.qs), mean.prop.income.reported,
               t(perc.hideable.income.reported.qs), mean.perc.hideable.income.reported,
               t(per.audit.rate.qs), mean.per.audit.rate,
               t(per.penalty.rate.qs), mean.per.penalty.rate,
               sd.prop.income.reported, sd.perc.hideable.income.reported, sd.per.audit.rate, sd.per.penalty.rate,
               netw.effect.report.gradient, netw.effect.report.r.sqrd,
               netw.effect.hideable.reported.gradient, netw.effect.hideable.reported.r.sqrd,
               netw.effect.per.audit.rate.gradient, netw.effect.per.audit.rate.r.sqrd,
               netw.effect.compliance.gradient, netw.effect.compliance.r.sqrd, asrt.means,
               persistently.compliant, persistently.non.compliant, total.persistence))
}