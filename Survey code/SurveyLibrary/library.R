#LIB <- new.env()
#LIB$library.dir <- library.dir
#l <- list.files(path=library.dir)
#l <- l[grep("\\.R",l)]
#tilde <- grep("\\.R~",l)
#if (length(tilde) > 0) l <- l[-tilde]
#l <- l[l != "library.R"]
#s <- sapply(l,FUN=function(x,library.dir, env) {
#  sys.source(file.path(library.dir,x), env)},
#            library.dir, env=LIB)
#rm(s,l,tilde)



library.dir <- 'Survey code/SurveyLibrary/'
l <- list.files(path=library.dir)
l <- l[grep("\\.R",l)]
tilde <- grep("\\.R~",l)
if (length(tilde) > 0) l <- l[-tilde]
l <- l[l != "library.R"]
s <- sapply(l,FUN=function(x,library.dir) { sys.source(file.path(library.dir,x), .GlobalEnv)}, library.dir)
rm(s,l,tilde)

# sourceCpp("Library/cpp.get.network.interactions.cpp")
# sourceCpp("Library/cpp.do.network.interactions.cpp")
# sourceCpp("Library/cpp.get.nn.int.cpp")