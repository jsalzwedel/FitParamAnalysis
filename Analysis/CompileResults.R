

# Load in the results of the analyses
rmsStdDev <- read.csv("SystematicsCutVariationErrors.csv")
sepData <- read.csv("FitTableSepFit.csv", row.names = 1)

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
