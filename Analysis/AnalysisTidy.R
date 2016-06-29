# Read in the data and clean up names
setwd("~/Analysis/lambda/AliAnalysisLambda/Fitting/FemtoFitting/FitResults/RAnalysis/Analysis")
dataRaw <- read.csv("../FitTable-2016-06-28-CFWeightCombine.csv")
cleanNames <- gsub(".", "", colnames(dataRaw), fixed = TRUE)
colnames(dataRaw) <- cleanNames
summary(dataRaw)

# Remove the empty rows (trailing whitespace in the spreadsheet)
data <- dataRaw[rowSums(is.na(dataRaw)) != ncol(dataRaw),]

# Remove the LLAA Im(f0) columns
data <- data[,!names(data) %in% c("ImF0LLAA010", "ImF0LLAA010Err")]

# We won't use any analyses with LLAA 30-50% cent. Remove rows where they were
# included, then remove those columns.
data <- data[is.na(data$RadiusLLAA3050),]
ll3050Cols <- grepl("LLAA3050", colnames(data))
data <- data[!ll3050Cols]

# Separate the global fits (radii constrained to be the same for LLAA and LA) 
# from the separate fits (different radii)
# Find the fits (rows) where both LLAA and LA are included, but RadiusLA is NA
rowGlobal <- with(data, !is.na(RadiusLLAA010) 
                  & !is.na(ReF0LA010) 
                  & is.na(RadiusLA010))
dataGlobal <- data[rowGlobal,]
dataSep <- data[!rowGlobal,] 

# Write the cleaned up data
write.csv(dataGlobal, file = "CleanGlobalFits.csv")
write.csv(dataSep, file = "CleanSepFits.csv")

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

