
ALP.combineIncome <- function(X1, X2, categorical =TRUE ) {
  N1 <- rep(0, each = length(X1))
  
  if(categorical){
    ordering <-rep(0, each = length(X1))
    for (i in 1:length(X1)) {
      if (!(is.na(X1[i]) | (X1[i] == 14 & is.na(X2[i])))) {
        # Less than $5,000
        {if (X1[i] == 1) {
          N1[i] <- "Less than $5,000"
          ordering[i] <- 1
        }}
        # $5,000 to $9,999
        {if (X1[i] == 2 | X1[i] == 3) {
          N1[i] <- "$5,000 to $9,999"
          ordering[i] <- 2
        }}
        # $10,000 to $14,999
        {if (X1[i] == 4 | X1[i] == 5) {
          N1[i] <- "$10,000 to $14,999"
          ordering[i] <- 3
        }}
        # $15,000 to $19,999
        {if (X1[i] == 6) {
          N1[i] <- "$15,000 to $19,999"
          ordering[i] <- 3
        }}
        # $20,000 to $24,999
        {if (X1[i] == 7) {
          N1[i] <- "$20,000 to $24,999"
          ordering[i] <- 4
        }}
        # $25,000 to $29,999
        {if (X1[i] == 8) {
          N1[i] <- "$25,000 to $29,999"
          ordering[i] <- 5
        }}
        # $30,000 to $34,999
        {if (X1[i] == 9) {
          N1[i] <- "$30,000 to $34,999"
          ordering[i] <- 6
        }}
        # $35,000 to $39,999
        {if (X1[i] == 10) {
          N1[i] <- "$35,000 to $39,999"
          ordering[i] <- 7
        }}
        # $40,000 to $49,999
        {if (X1[i] == 11) {
          N1[i] <- "$40,000 to $49,999"
          ordering[i] <- 8
        }}
        # $50,000 to $59,999
        {if (X1[i] == 12) {
          N1[i] <- "$50,000 to $59,999"
          ordering[i] <- 9
        }}
        # $60,000 to $74,999
        {if (X1[i] == 13) {
          N1[i] <- "$60,000 to $74,999"
          ordering[i] <- 10
        }}
        # $75,000 to $99,999
        {if (X1[i] == 14 & X2[i] == 1) {
          N1[i] <- "$75,000 to $99,999"
          ordering[i] <- 11
        }}
        # $100,000 to $124,999
        {if (X1[i] == 14 & X2[i] == 2) {
          N1[i] <- "$100,000 to $124,999"
          ordering[i] <- 12
        }}
        # $125,000 - $199,999
        {if (X1[i] == 14 & X2[i] == 3) {
          N1[i] <- "$125,000 - $199,999"
          ordering[i] <- 13
        }}
        # $200,000 or more
        {if (X1[i] == 14 & X2[i] == 4) {
          N1[i] <- "$200,000 or more"
          ordering[i] <- 14
        }}
      }
      else {N1[i] <- NA}
    }
    
    N1<- reorder(as.factor(N1),ordering,order =TRUE)
  }else{
    # function to combine income1 & income2 into one variable with dollars intead of categories
    # using CPS_2015_family_income.csv
    # there are a bunch of cases where income1 != 14, but income2 has a value
    # respondent must have made more money in a previous incarnation of the hhbox
    for (i in 1:length(X1)) {
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
  }
  
  return(N1)
}

