# Read in the data and clean up names
setwd("~/Analysis/lambda/AliAnalysisLambda/Fitting/FemtoFitting/FitResults/RAnalysis/Cleaning")
dataRaw <- read.csv("../FitTable-2016-07-05-GlobalImf0.csv")
cleanNames <- gsub(".", "", colnames(dataRaw), fixed = TRUE)
colnames(dataRaw) <- cleanNames
summary(dataRaw)

# Remove the empty rows (trailing whitespace in the spreadsheet)
data <- dataRaw[rowSums(is.na(dataRaw)) != ncol(dataRaw),]

# Remove the LLAA Im(f0) columns
data <- data[,!names(data) %in% c("ImF0LLAA010", "ImF0LLAA010Err",
                                  "ImF0LLAA1030", "ImF0LLAA1030Err",
                                  "ImF0LLAA3050", "ImF0LLAA3050Err")]

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

# Find the fits with Imf0 fixed to 1
rowImF0Fixed <- with(data, !is.na(ImF0LA010) & is.na(ImF0LA010Err))

# Split up the data
dataGlobalNoFix <- data[rowGlobal & !rowImF0Fixed,]
dataGlobalFixed <- data[rowGlobal & rowImF0Fixed,]
dataSepNoFix <- data[!rowGlobal & !rowImF0Fixed,] 
dataSepFixed <- data[!rowGlobal & rowImF0Fixed,]

# Remove Trials 31-33 (rows 1-3 of dataGlobalFixed) because wrong fit range
dataGlobalFixed <- dataGlobalFixed[c(4:6),]

# Write the cleaned up data
write.csv(dataGlobalNoFix, file = "GlobalNoImF0FixFits.csv")
write.csv(dataGlobalFixed, file = "GlobalImF0FixedFits.csv")
write.csv(dataSepNoFix, file = "SepNoImF0FixFits.csv")
write.csv(dataSepFixed, file = "SepImF0FixedFits.csv")
