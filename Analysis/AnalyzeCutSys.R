# Analyze cut systematics fits

setwd("~/Analysis/lambda/AliAnalysisLambda/Fitting/FemtoFitting/FitResults/RAnalysis/Analysis")

# Read in the clean data set

sysData <- read.csv("../Cleaning/CleanCutSysData.csv")


RMS <- function(vals) {
    # Remove na
    clean <- na.omit(vals)
    quad <- clean^2
    quadMean <- mean(quad)
    sqrt(quadMean)
}

mean <- sapply(sysData[c(1:54)], mean)
rms <- sapply(sysData[c(1:54)], RMS)

rmsDiff <- sqrt(rms^2 - mean^2)

RMSDiff <- function(vals) {
    clean <- na.omit(vals)
    sqrt(RMS(vals)^2 - mean(vals)^2)
}

SumQuad <- function(vals) {
    clean <- na.omit(vals)
    sqrt(sum(vals^2))
}



# Separate the value columns from the error columns
errColIndices <- grepl("Err",colnames(sysData))
errors <- sysData[errColIndices]
values <- sysData[!errColIndices]

rmsDiffByStudy <- with(values, aggregate(values, by = list(StudyType, VarIndex), RMSDiff))

# Different ways to quantify combined sys errors
maxRMSDiff <- sapply(rmsDiffByStudy[, !colnames(rmsDiffByStudy) %in% c("StudyType", "Group.1", "Group.2")], max)
rmsDiffAll <- sapply(values[!colnames(values) %in% c("StudyType")], RMSDiff)
rmsStdDev <- sapply(rmsDiffByStudy[,c(3:29)], RMS)   # <- Preferred?
quadStdDev <- sapply(rmsDiffByStudy[,c(3:29)], SumQuad)


# Tidy up the output
rmsStdDevOutput <- data.frame(Parameter = names(rmsStdDev), SigmaCut = rmsStdDev)
rmsStdDevOutput$Parameter <- gsub("LamALam","LA", rmsStdDevOutput$Parameter)
write.csv(rmsStdDevOutput, "SystematicsCutVariationErrors.csv", row.names = FALSE)


