
# Separate the value columns from the error columns
errColIndices <- grepl("Err",colnames(data))
errors <- data[errColIndices]
values <- data[!errColIndices]


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

write.csv(results, file = "FitTableGlobalOnly.csv")
