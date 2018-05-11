remove(list = ls(all = TRUE))

#library(doParallel)
library(plyr)
source("TaxModel.R")

calib.dir <- "Outputs/Calibration/"

calibration.analysis <- function(random.seed.file = "Inputs/random.seed.table.csv",
                                 test.cases.file = "Inputs/lhs.cases_calibration.csv",
                                 start.model = 1,
                                 end.model = 2,
                                 start.case=2,
                                 end.case = -1,
                                 type = "PSOCK",
                                 outfile="calibration_output.csv", 
                                 logfile = "Outputs/Calibration/log.txt",
                                 parallelize.runs = F)
{
  set.seed(55279) ### first seed given by Chris Marcum
  set.seed(55333) 
  
  sink(logfile)
  #Parallelization setup
  if(parallelize.runs) {
    num.of.cores <- min(detectCores(), 2) #max(1, detectCores()-1)
    cl <- makeCluster(num.of.cores, outfile="", type=type)
    registerDoParallel(cl)
  } else {
    cl <- NULL
  }
  
  random.seed.table <- read.csv(random.seed.file)
  all.test.cases    <- read.csv(test.cases.file, stringsAsFactors = F)
  #Removing case 0 as we don't need it any longer
  all.test.cases <- all.test.cases[-1, ]
  
  #n.model.options <- end.model #max(all.test.cases$model.option)
  model.results <- NULL
  config.names <- names(all.test.cases)
  l <- length(config.names)
  
  for(case.i in start.case:end.case) {
    
    print(paste("Running Case.i: ", case.i))
    retval.0 <- NULL
    config.for.case.i <- prepare.config(all.test.cases[case.i, ])
    seed.i = 0
    final.state <- NULL
    fy <- Inf
    case.results <- NULL
    for(seed in random.seed.table$random.seed)
    {
      if(seed.i == 0) {
        print("Running it till equilibrium")
        config.for.case.0 <- prepare.config(all.test.cases[case.i, ]) 
        retval.0 <- tryCatch({
          config.for.case.0[config.for.case.0$config.vars == 'run.till.equilibrium', 'value'] <- TRUE
          tax.model(config = config.for.case.0, return.final.state.not.outputs = TRUE, cluster = cl)
        },
        error = function(e){
          print(paste("Model failed for case 0 on model option: ", case.i))
        })
        
        final.state <- retval.0[['final.state']]
        fy <- unique(final.state$t)
        if(fy >= 1000) { #The model runs till 999 years to find equilibrium
          print(paste0("Case i: ", case.i, ", Note: This Case Did Not Reach Equilibrium!!"))
          #break
        } else {
          print(paste("Equilibrium reached at t:", fy, "for case:", case.i))
        }
      }
      
      seed.i <- seed.i + 1
      retval.n <- NULL
      outputs <- NULL
      
      retval.n <- tryCatch({
        #browser()
        tax.model(config = config.for.case.i, seed=seed, initial.state = final.state, cluster = cl)
      }, error = function(e){
        print(paste("Model failed for case", case.i, "on model option:", i))
      }, finally = {
        print(paste("Seed.i:", seed.i, "Seed:", seed))
      })
      
      outputs <- retval.n[['outputs']]
      case.results <- rbind.fill(case.results, cbind.data.frame(t(config.for.case.i$value), seed=seed, outputs))
      final.state.file.name <- paste0(calib.dir, "tax_model_", case.i, "_", seed.i, "_seed_value_", seed, ".csv")
      write.csv(retval.n[['final.state']], file=final.state.file.name)
    }
    
    if(!is.null(case.results)) {
      names(case.results)[1:l] <- config.names
      model.results <- rbind.fill(model.results, case.results)
    }
    results <- as.data.frame(model.results)
    write.csv(file=outfile, results)
  }
  
  sink()
  
  if(parallelize.runs)
    stopCluster(cl)
  
  return(model.results)
}

res <- calibration.analysis(start.model=1, end.model=1, start.case = 1, end.case = 700,
                                                 outfile=paste0(calib.dir,"1-to-700.csv"),
                                                 logfile = paste0(calib.dir,"log_1-to-700.txt"), parallelize.runs = T)

res <- calibration.analysis(start.model=1, end.model=1, start.case = 701, end.case = 1400,
                            outfile=paste0(calib.dir,"701-to-1400.csv"),
                            logfile = paste0(calib.dir,"log_701-to-1400.txt"), parallelize.runs = T)

res <- calibration.analysis(start.model=1, end.model=1, start.case = 1401, end.case = 2000,
                            outfile=paste0(calib.dir,"1401-to-2000.csv"),
                            logfile = paste0(calib.dir,"log_1401-to-4000.txt"), parallelize.runs = T)
