qP.parameters.list <- get.qP.lognormal.fit_v2(df.hypo,c1.field ="c1.guessed",
                                              c2=0.7,ET.slope=ET.slope.c1.tilde)
qP.parameters.list$prim_key <- rownames(qP.parameters.list)

dim(qP.parameters.list)

qP.parameters1 <- rbind(s=c(summary(qP.parameters.list[,1])),
                       m=c(summary(qP.parameters.list[,2])))

print(qP.parameters1)

qP.parameters.list <- get.qP.lognormal.fit_v2(df.hypo,c1.field ="c1.guess.majority.int",
                                              c2=0.7,ET.slope=ET.slope.c1.tilde)
qP.parameters.list$prim_key <- rownames(qP.parameters.list)

dim(qP.parameters.list)

qP.parameters2 <- rbind(s=c(summary(qP.parameters.list[,1])),
                        m=c(summary(qP.parameters.list[,2])))

print(qP.parameters2)

qP.parameters.list <- get.qP.lognormal.fit_v2(df.hypo,c1.field ="tax.rate.threshold.tri",
                                              c2=0.7,ET.slope=ET.slope.c1.tilde)
qP.parameters.list$prim_key <- rownames(qP.parameters.list)

dim(qP.parameters.list)

qP.parameters3 <- rbind(s=c(summary(qP.parameters.list[,1])),
                        m=c(summary(qP.parameters.list[,2])))

print(qP.parameters3)

print(xtable(qP.parameters3,digits=3),  
      file=paste(survey.output.dir ,"qP_parameters_tridist",".tex",sep=""))
