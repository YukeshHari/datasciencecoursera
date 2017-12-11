corr <- function(directory, threshold = 0) {
    completeCases <- complete(directory)
  casesAboveThreshold <- completeCases[completeCases$nobs > threshold,1]
  allFiles <- list.files(path = directory, full.names = TRUE)
  correlations <- rep(NA,length(casesAboveThreshold))
  for (i in casesAboveThreshold) {
    fileData <- (read.csv(allFiles[i]))
    completeCases <- complete.cases(fileData)
    validSulfateData <- fileData[completeCases, 2]
    validNitrateData <- fileData[completeCases, 3]
    correlations[i] <- cor(x = validSulfateData, y = validNitrateData)
   
  }
  correlations <- correlations[complete.cases(correlations)]
}