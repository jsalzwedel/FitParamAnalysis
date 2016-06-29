# Script to analyze the fit results in cleaned up data sets (from ../Cleaning)

setwd("~/Analysis/lambda/AliAnalysisLambda/Fitting/FemtoFitting/FitResults/RAnalysis/Analysis")

Analyze <- function(data, outputName) {

    # Separate the value columns from the error columns
    errColIndices <- grepl("Err",colnames(data))
    errors <- data[errColIndices]
    values <- data[!errColIndices]
    
    # Some of the columns might be NA. If either the value column or the error
    # column is NA, remove the pair from errors and values
    errNotNA <- colSums(is.na(errors)) != nrow(errors)
    valNotNA <- colSums(is.na(values)) != nrow(values)
    errors <- errors[errNotNA & valNotNA]
    values <- values[errNotNA & valNotNA]

    # Find the mean, RMS, min, and max of the data
    RMS <- function(vals) {
        # Remove na
        clean <- na.omit(vals)
        quad <- clean^2
        quadMean <- mean(quad)
        sqrt(quadMean)
    }
    means <- colMeans(values, na.rm = TRUE)
    rms <- sapply(values, RMS)
    max <- sapply(values, max, na.rm=TRUE)
    min <- sapply(values, min, na.rm=TRUE)
    
    # Calculate the mean error and the rms error
    errMeans <- colMeans(errors, na.rm = TRUE)
    errRMS <- sapply(errors, RMS)
    
    # Combine everything together
    resultsAll <- data.frame(Type = colnames(values), Mean = as.numeric(means), 
                            RMS = rms, Max = max, Min = min, 
                            MeanError = errMeans, RMSError = errRMS)
    resultsAll$Sigma <- sqrt(resultsAll$RMS^2 - resultsAll$Mean^2)

    # Most of the time, we won't really care about the normalization
    # or the bkg coefficients. Let's just take radius, Ref0, Imf0, and d0.
    results <- resultsAll[grep("Rad|ReF0|ImF0LA|D0", rownames(resultsAll)), ]

    outputFileName = paste("FitTable",outputName, ".csv", sep="")
    write.csv(results, file = outputFileName)
}

# Analyze both the global fit data and separate fit data
dataSep <- read.csv("../Cleaning/CleanSepFits.csv", row.names=1)
dataGlobal <- read.csv("../Cleaning/CleanGlobalFits.csv", row.names=1)


Analyze(dataSep,"SepFit")
Analyze(dataGlobal,"GlobalFit")
