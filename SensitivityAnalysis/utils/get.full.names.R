#Returns the full form of all variable names used to represent
#inputs and outputs in the Model

get.full.names <- function(filename = "SensitivityAnalysis/full_io_names.csv") {
  df <- read.csv(filename, stringsAsFactors = F)
  all.io.names <- df$full.names
  names(all.io.names) <- df$field.name
  
  return(all.io.names)
}
  #Hard coding these field names here, assuming we will not be adding more model options.
  # all.names <- NULL
  # 
  # #Model Options
  # all.names["network.model"] <- "Network Effect"
  # all.names["mild.tendency.to.full.evasion"] <- "Mild Tendency to Full Evasion"
  # all.names["tax.refund.effect"] <- "Tax Refund Effect"
  # all.names["targetted.auditing"] <- "Targeted Auditing"
  # all.names["bomb.crater.effect"] <- "Bomb Crater Effect"
  # all.names["gamblers.fallacy"] <- "Gambler's Fallacy"
  # all.names["media.effect"] <- "Media Effect"
  # 
  # #Important MOdel Inputs
  # all.names['tax.rate'] <- "Tax Rate"
  # all.names['tax.rate.delta'] <- "Changes in Tax Rate"
  # all.names['audit.rate'] <- "Audit Rate"
  # all.names['penalty.rate'] <- "Penalty Rate"
  # all.names['detection.eff'] <- "Detection Efficiency"
  # all.names['K'] <- "Years of Audit Memory (K)"
  # all.names['beta.media'] <- "Beta Media"
  # all.names['media.mid.effect'] <- "Media Mid Effect"
  # all.names['media.steepness'] <- "Media Steepness"
  # all.names["media.stochastic.offset"] <- "Media Stochastic Offset"
  # all.names["tax.gap.reporting.media.threshold"] <- "Media Reporting Threshold on Tax Gap"
  # all.names["t.media.range"] <- "T.Media.Range"
  # all.names['morale.half.life'] <- "Half-life of Morale"
  # all.names['generation.half.life'] <- "Half-life of Generation"
  # all.names['penalty.asymmetry.factor']<-'penalty.asymmetry.factor'
  # all.names['return.weight']<-'Tax Returns Weight'
  # all.names["rate.refund.movement"] <- "Rate of Refund Movement"
  # all.names['beta.personal'] <- 'Beta Personal'
  # all.names['full.tendency.factor'] <- "Full Tendency Factor"
  # 
  # all.names['c2']<-'c2'
  # all.names['ave.degree.tTaxes']<-'Average Degree of (Tax) Network'
  # all.names['beta.network']<-'Network Morale'
  # 
  # #all.names["mu"] <- "mu"
  # 
  # all.names['bomb.crater.factor'] <- "Bomb Crater Factor"
  # all.names["gamblers.fallacy.grad"] <- "Gambler's Fallacy Gradient"
  # all.names["gamblers.fallacy.intercept"] <- "Gambler's Fallacy Intercept"
  # all.names["m.qP"] <- "Deterrence response (m.qP)"
  # all.names["s.qP"] <- "Deterrence response(s.qP)"
  # all.names["v.PP"] <- "Evasion Relapse"
  # all.names["c1.dist.weight"] <- "c1 Distribution Weights"
  # 
  # all.names['letter.rate']<-'Letter rate'
  # all.names['tax.complexity']<-'Tax Complexity'
  # #all.names['f.max.percieved']<-'f Max Percieved'
  # all.names['per.prop.of.penalty.rate']<-'PerProp of PenaltyRate'
  # 
  # #Network Effect specific parameters
  # #all.names['yearly.prop.interactions'] <- "Yearly Proportion of Interactions"
  # all.names["netw.effect.compliance.gradient"] <- "Network Effect on Compliance"
  # all.names['netw.effect.hideable.reported.gradient'] <- "Network Effect on H.I. Reporting"
  # all.names['per.prop.of.penalty.rate'] <- "Perceived Proportion of Penalty Rate"
  # 
  # #Important Model Outputs
  # all.names["comp.rate.rmse"]  <- "Under-reporting Distribution matches Observed"
  # all.names["prop.income.reported.q.5."] <- "Zero Income Reported"
  # all.names["prop.income.reported.q.100."] <- "Full Income Reported"
  # all.names["perc.hideable.income.reported.q.5."] <- "Zero Hideable Income Reported"
  # all.names["perc.hideable.income.reported.q.100."] <- "Full Hideable Income Reported"
  # all.names['mean.per.penalty.rate'] <- "Mean Perceived Penalty Rate"
  # all.names['mean.per.audit.rate'] <- "Mean Perceived Audit Rate"
  # all.names['tax.gap.percent'] <- "Mean Tax Gap"
  # 
  # all.names['sd.prop.income.reported'] <- "StdDev of Reproted Income"
  # all.names['sd.per.penalty.rate'] <- "StdDev of Perceived Penalty Rate"
  # all.names['sd.per.audit.rate'] <- "StdDev of Perceived Audit Rate"
  # 
  # all.names['R0'] <- "R0"
  # all.names['R1'] <- "R1"
  # all.names['R3'] <- "R3"
  # all.names['Rannoyed'] <- "Rannoyed"
  # all.names['hideable.Rannoyed'] <- "Rannoyed (Hideable)"
  # all.names['hideable.R0'] <- "R0 (Hideable)"
  # all.names['hideable.R1'] <- "R1 (Hideable)"
  # all.names['hideable.R3'] <- "R3 (Hideable)"
  # all.names['total.persistence'] <- "Persistence in Hideable Income Reporting"
  # 
  # all.names['other.inputs.combined'] <- "All Other Inputs Combined"
  # 
  # return(all.names)
