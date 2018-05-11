#Load all the utility functions.

utils.dir <- 'SensitivityAnalysis/utils/'
l <- list.files(path=utils.dir)
l <- l[grep("\\.R",l)]
tilde <- grep("\\.R~",l)
if (length(tilde) > 0) l <- l[-tilde]
l <- l[l != "utils.R"]
s <- sapply(l,FUN=function(x,utils.dir) { sys.source(file.path(utils.dir,x), .GlobalEnv)}, utils.dir)
rm(s,l,tilde)