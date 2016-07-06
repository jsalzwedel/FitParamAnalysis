# Analyze cut systematics fits

setwd("~/Analysis/lambda/AliAnalysisLambda/Fitting/FemtoFitting/FitResults/RAnalysis/Cleaning")


# Read in the column names from a separate file
colNames <- read.csv(file = "../SystematicColumnNames.csv", 
                     stringsAsFactors = FALSE,
                     header = FALSE)
names <- as.character(colNames[1,])

# Read in the systematics fit results
sysData <- read.csv(file = "../SystematicFitData.csv", header=FALSE)

colnames(sysData)<-names

# Omit the last row, row 48 (bad fit, D0LamALam < 0)
sysData <- head(sysData, 47)

write.csv(sysData, "CleanCutSysData.csv", row.names=FALSE)

