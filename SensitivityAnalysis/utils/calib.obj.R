calib.obj <- function(calibration.targets, observed, root.mean.squared = F) {
  
  sum.of.wts <- sum(calibration.targets$weight) 
  #print(paste("Sum of weights for calibration:", sum.of.wts))
  
  expected <- calibration.targets$value
  weights <- calibration.targets$weight
  
  calib.score <- (weights[1] * abs(observed[, 1] - expected[1]) +
                          weights[2] * abs(observed[, 2] - expected[2]) +
                          (weights[3] *(abs(observed[, 3] - expected[3]) +
                                        abs(observed[, 4] - expected[4]) +
                                        abs(observed[, 5] - expected[5]))/3) +
                          weights[6] * abs(observed[, 6] - expected[6]) +
                          weights[7] * abs(observed[, 7] - expected[7])
  )/sum.of.wts
  
  if(root.mean.squared) {
    calib.score <- sqrt((weights[1] * (observed[, 1] - expected[1])^2 +
                      weights[2] * (observed[, 2] - expected[2])^2 +
                      (weights[3] *((observed[, 3] - expected[3])^2 +
                                    (observed[, 4] - expected[4])^2 +
                                    (observed[, 5] - expected[5])^2)/3) +
                      weights[6] * (observed[, 6] - expected[6])^2 +
                      weights[7] * (observed[, 7] - expected[7])^2
    )/sum.of.wts)
  }
  
  
  # calib.score <- sqrt( (weights[1] * (observed[, 1] - expected[1])^2 +
  #                         weights[2] * (observed[, 2] - expected[2])^2 +
  #                         (weights[3] *((observed[, 3] - expected[3])^2 +
  #                                         (observed[, 4] - expected[4])^2 +
  #                                         (observed[, 5] - expected[5])^2)/3) +
  #                         (weights[6] * (observed[, 6] - expected[6])^2)
  # )/sum.of.wts
  # )
  
  return(calib.score)
}
