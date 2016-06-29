# Plot the fit parameter measurements from each trial

library(ggplot2)

PlotMeasurements <- function(data, valColName) {
    
    # Get the columns indices associated with valColName and its error column
    errColName <- paste(valColName,"Err", sep = "")
    valColIndex <- which(colnames(data) == valColName)
    errColIndex <- which(colnames(data) == errColName)
    
    # Subset the data to take just the non-NA rows of those two columns
    dataset <- data[!is.na(data[,valColIndex]), c(valColName, errColName)]
    colnames(dataset) <- c("Value","Error")
    # Setup the ggplot of the data
    g <- ggplot(dataset, aes(y = Value, x=as.numeric(rownames(dataset))))
    g <- g + geom_point(stat="identity", position = position_dodge(width=1))    
    gLimits <- aes(ymax = Value + Error,
                   ymin = Value - Error)
    g <- g + geom_errorbar(gLimits)
    g <- g + ggtitle(valColName)
    g
}

dataSep <- read.csv("../Cleaning/CleanSepFits.csv", row.names=1)
dataGlobal <- read.csv("../Cleaning/CleanGlobalFits.csv", row.names=1)

plots <- list()
for(name in colnames(dataSep)) {
    if(grepl("Err", name)) next
    print(name)
    plots <- c(plots, list(PlotMeasurements(dataSep, name)))
}

# Look just at d0 for LLAA
d0 <- data[!is.na(data$D0LLAA010), c("D0LLAA010", "D0LLAA010Err")]
gd0 <- ggplot(d0, aes(y=D0LLAA010, x=as.numeric(rownames(d0))))
gd0 <- gd0 + geom_point(stat="identity", position = position_dodge(width=1))
gd0Limits <- aes(ymax = D0LLAA010 + D0LLAA010Err, ymin = D0LLAA010 - D0LLAA010Err)
gd0 <- gd0 + geom_errorbar(gd0Limits)
gd0

ReF0 <- data[!is.na(data$ReF0LLAA010), c("ReF0LLAA010", "ReF0LLAA010Err")]
gReF0 <- ggplot(ReF0, aes(y=ReF0LLAA010, x=as.numeric(rownames(ReF0))))
gReF0 <- gReF0 + geom_point(stat="identity", position = position_dodge(width=1))
gReF0Limits <- aes(ymax = ReF0LLAA010 + ReF0LLAA010Err, ymin = ReF0LLAA010 - ReF0LLAA010Err)
gReF0 <- gReF0 + geom_errorbar(gReF0Limits)
gReF0

ReF0 <- data[!is.na(data$ReF0LA010), c("ReF0LA010", "ReF0LA010Err")]
gReF0 <- ggplot(ReF0, aes(y=ReF0LA010, x=as.numeric(rownames(ReF0))))
gReF0 <- gReF0 + geom_point(stat="identity", position = position_dodge(width=1))
gReF0Limits <- aes(ymax = ReF0LA010 + ReF0LA010Err, ymin = ReF0LA010 - ReF0LA010Err)
gReF0 <- gReF0 + geom_errorbar(gReF0Limits)
gReF0

Radius <- data[!is.na(data$RadiusLLAA010), c("RadiusLLAA010", "RadiusLLAA010Err")]
gRadius <- ggplot(Radius, aes(y=RadiusLLAA010, x=rownames(Radius)))
gRadius <- gRadius + geom_point(stat="identity", position = position_dodge(width=1))
gRadiusLimits <- aes(ymax = RadiusLLAA010 + RadiusLLAA010Err, ymin = RadiusLLAA010 - RadiusLLAA010Err)
gRadius <- gRadius + geom_errorbar(gRadiusLimits)
gRadius

Norm <- data[!is.na(data$NormLLAA010), c("NormLLAA010", "NormLLAA010Err")]
gNorm <- ggplot(Norm, aes(y=NormLLAA010, x=rownames(Norm)))
gNorm <- gNorm + geom_point(stat="identity", position = position_dodge(width=1))
gNormLimits <- aes(ymax = NormLLAA010 + NormLLAA010Err, ymin = NormLLAA010 - NormLLAA010Err)
gNorm <- gNorm + geom_errorbar(gNormLimits)
gNorm

d0 <- data[!is.na(data$D0LA010), c("D0LA010", "D0LA010Err")]
gd0 <- ggplot(d0, aes(y=D0LA010, x=rownames(d0)))
gd0 <- gd0 + geom_point(stat="identity", position = position_dodge(width=1))
gd0Limits <- aes(ymax = D0LA010 + D0LA010Err, ymin = D0LA010 - D0LA010Err)
gd0 <- gd0 + geom_errorbar(gd0Limits)
gd0