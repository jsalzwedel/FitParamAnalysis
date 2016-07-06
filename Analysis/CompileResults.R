
setwd("~/Analysis/lambda/AliAnalysisLambda/Fitting/FemtoFitting/FitResults/RAnalysis/Analysis")

# Load in the results of the analyses
rmsStdDev <- read.csv("SystematicsCutVariationErrors.csv")
sepData <- read.csv("FitTableSepFitFree.csv", row.names = 1)

# Clean up the data
results<- sepData[,c("Type","Mean","RMSError", "Sigma")]
names(results)[names(results) == "Type"] <- "Parameter"
names(results)[names(results) == "Sigma"] <- "SigmaFit"

# Merge the results
compiledResults <- merge(results, rmsStdDev, by = "Parameter", all = TRUE)

# Add on the combined errors column
compiledResults$SigmaTot <- sqrt(compiledResults$SigmaFit^2 
                                 + compiledResults$SigmaCut^2)

# Write the table
write.csv(compiledResults, "CompiledResults.csv", row.names=FALSE)

sepFitFree <- read.csv("FitTableSepFitFree.csv", row.names=1)
sepFitFixed <- read.csv("FitTableSepFitFixed.csv", row.names =1)
globalFitFree <- read.csv("FitTableGlobalFitFree.csv", row.names =1)
globalFitFixed <- read.csv("FitTableGlobalFitFixed.csv", row.names=1)


compiledResults
sepFitFree
sepFitFixed
globalFitFree
globalFitFixed