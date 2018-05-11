

remove(list = ls(all = TRUE))
gc()

library(xtable)
library(texreg)
library(yacca)
library(ggplot2)
library(reshape2)
require(Hmisc)
library(foreign)
library(corrgram)
library(xlsx)


## Specify directories 
survey.data.dir <- "Survey data/"
survey.output.dir <- "SurveyTablesPlots/"
library.dir   <- "Survey code/SurveyLibrary/"
library <- file.path(library.dir, "library.R")
source(library)

# get data
# ms456 <- read.csv(file = "C:/Users/colvin/Documents/R/Well_Being_4562016_07_28_10_22_31.csv")
ALP.data.file.name <- "Well_Being_4562016_09_02_10_54_58"
ms456  <- read.csv(paste(survey.data.dir,ALP.data.file.name,".csv",sep=""),stringsAsFactors = FALSE)

#ms456 <- read.csv("Survey data/Well_Being_4562016_07_28_10_22_31.csv")

# remove test household
ms456 <- ms456[!(ms456$prim_key%in%c("65:2")),]

# get needed variables
tim <- data.frame(selfemployed = ms456$selfemployed, age = ms456$calcage, gender = ms456$gender, income1 = ms456$familyincome, income2 = ms456$familyincome_part2, 
                  alter_se1 = ms456$taxselfemployed_1_, alter_se2 = ms456$taxselfemployed_2_, alter_se3 = ms456$taxselfemployed_3_, alter_se4 = ms456$taxselfemployed_4_, alter_se5 = ms456$taxselfemployed_5_,
                  alter_se6 = ms456$taxselfemployed_6_, alter_se7 = ms456$taxselfemployed_7_, alter_se8 = ms456$taxselfemployed_8_, alter_se9 = ms456$taxselfemployed_9_, alter_se10 = ms456$taxselfemployed_10_,
                  alter_ed1 = ms456$altereduc_1_, alter_ed2 = ms456$altereduc_2_, alter_ed3 = ms456$altereduc_3_, alter_ed4 = ms456$altereduc_4_, alter_ed5 = ms456$altereduc_5_, alter_ed6 = ms456$altereduc_6_, 
                  alter_ed7 = ms456$altereduc_7_, alter_ed8 = ms456$altereduc_8_, alter_ed9 = ms456$altereduc_9_, alter_ed10 = ms456$altereduc_10_)

# number of rows in the data field
nr <- nrow(tim)

# put the alter self-employed stuff into a matrix
alter_se_mat <- matrix(nrow = nr, ncol = 10)
alter_se_mat <- as.matrix(tim[, 6:15])

# function to get number of NA self-employed alters per ego
getNA <- function(X1) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    for (j in 1:10) {
      if (is.na(X1[i,j])) {
        N1[i] <- N1[i] + 1
      }
    }
  }
  N1
}

# get number of NA self-employed alters per ego
alter_se_na <- getNA(alter_se_mat)

# function to get number of self-employed alters per ego
getSE <- function(X1, X2) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    for (j in 1:10) {
      if (!is.na(X1[i,j]) & X1[i,j] == 1) {
        N1[i] <- N1[i] + 1
      }
    }
    # if all alters for this ego are NA, then NA
    if (X2[i] == 10) {
      N1[i] <- NA
    }
  }
  N1
}

# get number of self-employed alters (this is also the numerator for the proportion of alters that are self-employed)
alter_se_num <- getSE(alter_se_mat, alter_se_na)

# function to get number of non-NA alters per ego
getNonNA <- function(X1) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    N1[i] <- 10 - X1[i]
  }
  N1
}

# get number of non-NA alters per ego (this is also the denominator for the proportion of alters that are self-employed)
alter_se_denom <- getNonNA(alter_se_na)


# function to calculate proportion (number of self-employed / number of valid cases)
getProportion <- function(X1, X2) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    if (!is.na(X1[i])) {
      N1[i] <- X1[i] / X2[i]
    } else {
      N1[i] <- NA
    }
  }
  N1
}

# get proportion of alters that are self-employed
alter_se_proportion <- getProportion(alter_se_num, alter_se_denom)

# put alter_se_num, alter_se_denom & alter_se_proportion into the tim data frame
tim[, "alter_se_num"] <- alter_se_num
tim[, "alter_se_denom"] <- alter_se_denom
tim[, "alter_se_proportion"] <- alter_se_proportion

# Educational levels and Income in Liu pdf: http://homepage.divms.uiowa.edu/~kcowles/s138_2006/Liu_KuoFinalReport.pdf
#
# 1 Less than 9th Grade 13431
# 2 9th to 12th Grade (No Diploma) 16576
# 3 High School Graduate (Includes Equivalency) 23363
# 4 Some College, No Degree 29643
# 5 Associate Degree 32759
# 6 bachelor's degree 44962
# 7 master's degree 54421
# 8 Doctorate Degree 77195
# 9 Professional Degree 93358

# Educational levels in ms456
#
# 1 Less than a high school diploma or the equivalent (For example: GED)
# 2 High school diploma or the equivalent (For example: GED)
# 3 Associate degree in college
# 4 Bachelor's degree (For example: BA,AB,BS)
# 5 Graduate degree, such as Master's or Doctoral-level degree
# 6 Don't know

# assign income to the ms456 educational levels
#
# ms456[1] = (liu[1] + liu[2]) / 2 = 15004
# ms456[2] = liu[3] = 23363
# ms456[3] = liu[5] = 32759
# ms456[4] = liu[6] = 44962
# ms456[5] = (liu[7] + liu[8] + liu[9]) / 3 = 74991
# ms456[6] do not include these in the analysis

# put the education stuff into a matrix
alter_ed_mat <- matrix(nrow = nr, ncol = 10)
alter_ed_mat <- as.matrix(tim[, 16:25])

# function to get total income based on education
getIncome <- function(X1) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    for (j in 1:10) {
      if (!is.na(X1[i,j])) {
        {if (X1[i,j] == 1) {N1[i] <- N1[i] + 15004}}
        {if (X1[i,j] == 2) {N1[i] <- N1[i] + 23363}}
        {if (X1[i,j] == 3) {N1[i] <- N1[i] + 32759}}
        {if (X1[i,j] == 4) {N1[i] <- N1[i] + 44962}}
        {if (X1[i,j] == 5) {N1[i] <- N1[i] + 74991}}
      }
    }
    if(N1[i] == 0) {N1[i] <- NA}
  }
  N1
}

# get total income based on education
alter_ed_income <- getIncome(alter_ed_mat)

# function to count number of valid educations (1 >= edu <= 5) (so we can compute averages with ED_income)
getValidED <- function(X1) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    for (j in 1:10) {
      if (!is.na(X1[i,j])) {
        if (X1[i,j] >= 1 & X1[i,j] <= 5) {
          N1[i] <- N1[i] + 1
        }
      }
    }
  }
  N1
}

# get number of valid educations (this will be the denominator for the average income)
alter_ed_income_denom <- getValidED(alter_ed_mat)

# function to calculate average income
getAverageIncome <- function(X1, X2) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    if (!is.na(X1[i]) & X2[i] != 0) {
      N1[i] = X1[i] / X2[i]
    } else {N1[i] <- NA}
  }
  N1
}

# get average income
alter_ed_income_average <- getAverageIncome(alter_ed_income, alter_ed_income_denom)

# put alter_ed_income, alter_ed_income_denom & alter_ed_income_average into the tim data frame
tim[, "alter_ed_income"] <- alter_ed_income
tim[, "alter_ed_income_denom"] <- alter_ed_income_denom
tim[, "alter_ed_income_average"] <- alter_ed_income_average

# replace the 2s in gender with 0s (female)
tim[, 3][tim[, 3] == 2] <- 0

# replace the 1s and 3s in selfemployed with 0 (not self_employed)
tim[, 1][tim[, 1] == 1 | tim[, 1] == 3] <- 0

# replace the 2s in selfemployed with 1 (self_employed)
tim[, 1][tim[, 1] == 2] <- 1

# function to combine income1 & income2 into one variable with dollars intead of categories
# using CPS_2015_family_income.csv
# there are a bunch of cases where income1 != 14, but income2 has a value
# respondent must have made more money in a previous incarnation of the hhbox
combineIncome <- function(X1, X2) {
  N1 <- rep(0, each = nr)
  for (i in 1:nr) {
    if (!(is.na(X1[i]) | (X1[i] == 14 & is.na(X2[i])))) {
      # Less than $5,000
      {if (X1[i] == 1) {N1[i] <- 1249}}
      # $5,000 to $9,999
      {if (X1[i] == 2 | X1[i] == 3) {N1[i] <- 7927}}
      # $10,000 to $14,999
      {if (X1[i] == 4 | X1[i] == 5) {N1[i] <- 12388}}
      # $15,000 to $19,999
      {if (X1[i] == 6) {N1[i] <- 17278}}
      # $20,000 to $24,999
      {if (X1[i] == 7) {N1[i] <- 22165}}
      # $25,000 to $29,999
      {if (X1[i] == 8) {N1[i] <- 27186}}
      # $30,000 to $34,999
      {if (X1[i] == 9) {N1[i] <- 32085}}
      # $35,000 to $39,999
      {if (X1[i] == 10) {N1[i] <- 37183}}
      # $40,000 to $49,999
      {if (X1[i] == 11) {N1[i] <- (42013 * 0.0473867360953127 + 47198 * 0.0415645037621226) / (0.0473867360953127 + 0.0415645037621226)}}
      # $50,000 to $59,999
      {if (X1[i] == 12) {N1[i] <- (51984 * 0.0428705037789742 + 57154 * 0.0354136649730795) / (0.0428705037789742 + 0.0354136649730795)}}
      # $60,000 to $74,999
      {if (X1[i] == 13) {N1[i] <- (61941 * 0.0371746585441891 + 67095 * 0.0301559616794318 + 72042 * 0.031756864925895) / (0.0371746585441891 + 0.0301559616794318 + 0.031756864925895)}}
      # $75,000 to $99,999
      {if (X1[i] == 14 & X2[i] == 1) {N1[i] <- (77007 * 0.0262716648551183 + 81979 * 0.0264823100191266 + 87142 * 0.0225811615816924 + 92009 * 0.0211993293057978 + 97155 * 0.0177784518423026) / (0.0262716648551183 + 0.0264823100191266 + 0.0225811615816924 + 0.0211993293057978 + 0.0177784518423026)}}
      # $100,000 to $124,999
      {if (X1[i] == 14 & X2[i] == 2) {N1[i] <- (101830 * 0.0210476647877118 + 107162 * 0.0149810840642721 + 111973 * 0.0150147872905134 + 117204 * 0.0124701937092928 + 121842 * 0.0123859356436895) / (0.0210476647877118 + 0.0149810840642721 + 0.0150147872905134 + 0.0124701937092928 + 0.0123859356436895)}}
      # $125,000 - $199,999
      {if (X1[i] == 14 & X2[i] == 3) {N1[i] <- (127026 * 0.0104732775544939 + 132066 * 0.0104142969085716 + 137202 * 0.0089145033408323 + 141989 * 0.00820673558976433 + 146959 * 0.00659740653674073 + 151788 * 0.00828256784880733 + 157072 * 0.00565371620198343 + 162151 * 0.00535881297237178 + 167076 * 0.00455836134914015 + 171948 * 0.00467632264098481 + 177161 * 0.00397698069647717 + 181956 * 0.00390957424399451 + 187307 * 0.00282264519771155 + 192029 * 0.00301643874859921 + 197119 * 0.00244348390249657) / (0.0104732775544939 + 0.0104142969085716 + 0.0089145033408323 + 0.00820673558976433 + 0.00659740653674073 + 0.00828256784880733 + 0.00565371620198343 + 0.00535881297237178 + 0.00455836134914015 + 0.00467632264098481 + 0.00397698069647717 + 0.00390957424399451 + 0.00282264519771155 + 0.00301643874859921 + 0.00244348390249657)}}
      # $200,000 or more
      {if (X1[i] == 14 & X2[i] == 4) {N1[i] <- (219377 * 0.0180565034587936 + 398194 * 0.0209297034958671) / (0.0180565034587936 + 0.0209297034958671)}}
    }
    else {N1[i] <- NA}
  }
  N1
}

# combine income1 & income2 into one variable (ego_income_new) with dollars intead of categories
ego_income_new <- combineIncome(tim$income1, tim$income2)

# put ego_income_new into the tim data frame
tim[, "ego_income_new"] <- ego_income_new


tim$income.val.5 <- ego_income_new/10^5
tim$alter_income.val.5 <- tim$alter_ed_income_average/10^5

# regression without alter variables
# use glm to do a logistic regression where selfemployed depends on gender, age & ego_income_new
lrfit0 <- glm(formula = selfemployed ~ gender + age + income.val.5, data=tim, family = binomial)

summary(lrfit0)


# regression with 1 alter variable
# use glm to do a logistic regression where selfemployed depends on gender, age, ego_income_new & alter_ed_income_average
lrfit1 <- glm(formula = selfemployed ~ gender + age + income.val.5 + alter_income.val.5, data= tim, family = binomial)

summary(lrfit1)

# regression with alter variables
# use glm to do a logistic regression where selfemployed depends on gender, age, ego_income_new, alter_se_proportion & alter_ed_income_average
lrfit2 <- glm(formula = selfemployed ~ gender + age + income.val.5 + alter_se_proportion + alter_income.val.5, data= tim, family = binomial)

summary(lrfit2)






reg.out <- texreg(list(lrfit0,lrfit1,lrfit2),
       caption="Regression Models predicting self-employment status",
       dcolumn=FALSE,
       override.se=list(summary(lrfit0)$coef[,2],
                        summary(lrfit1)$coef[,2],
                        summary(lrfit2)$coef[,2]),
       override.pval=list(summary(lrfit0)$coef[,4],
                          summary(lrfit1)$coef[,4],
                          summary(lrfit2)$coef[,4]))


print(reg.out,  
      file=paste(survey.output.dir ,"Regression_Self_Employed",".tex",sep=""))


# regression with 1 alter variable
# use glm to do a logistic regression where selfemployed depends on gender, age, ego_income_new & alter_ed_income_average
lrfit1 <- glm(formula = selfemployed ~ gender + age , data= tim, family = binomial)

summary(lrfit1)

# regression with alter variables
# use glm to do a logistic regression where selfemployed depends on gender, age, ego_income_new, alter_se_proportion & alter_ed_income_average
lrfit2 <- glm(formula = selfemployed ~ gender + age  + alter_se_proportion , data= tim, family = binomial)

summary(lrfit2)






reg.out2 <- texreg(list(lrfit1,lrfit2), center=F,
                  caption="Consolidated Regression Models predicting self-employment status",
                  dcolumn=FALSE,
                  override.se=list(summary(lrfit1)$coef[,2],
                                   summary(lrfit2)$coef[,2]),
                  override.pval=list(summary(lrfit1)$coef[,4],
                                     summary(lrfit2)$coef[,4]))


print(reg.out2,  
      file=paste(survey.output.dir ,"Regression_Self_Employed_consolidated",".tex",sep=""))


se.reg.models <- list(se.reg1=lrfit1,se.reg2=lrfit2)
saveRDS(se.reg.models,file=paste(survey.data.dir,"se.reg.models",".Rdata",sep=""))











