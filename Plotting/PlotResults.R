results <- read.csv("FitTableSepFit.csv")


library(ggplot2)
gMeans <- ggplot(results, aes(x=Type, y=Mean))
gMeans <- gMeans+geom_point(stat="identity",position=position_dodge(width = 1))
limits <- aes(ymax = Max, ymin = Min)

# Add on the systematic error bars (Max/Min values)
# gMeans <- gMeans + geom_errorbar(limits,
#                                  color = "red")

# Also add in std deviation error bar
gMeans <- gMeans +geom_errorbar(aes(ymax = Mean + Sigma, ymin = Mean - Sigma),
                                position = position_dodge(width = 1),
                                width = 1,
                                color = "blue")

# Add in statistical error bars (mean error)
# gMeans <- gMeans + geom_errorbar(aes(ymax = Mean + MeanError, ymin = Mean - MeanError),
#                                 position=position_dodge(width = 1),
#                                 width = 0.5,
#                                 color = "red")

# Add in statistical error bars (rms error)
gMeans <- gMeans + geom_errorbar(aes(ymax = Mean + RMSError, ymin = Mean - RMSError),
                                 position=position_dodge(width = 1),
                                 width = 0.5,
                                 color = "black")

# Tidy up the labels
gMeans <- gMeans + theme(text = element_text(size=20),
                         axis.text.x = element_text(angle=90, vjust=1))

gMeans <- gMeans + labs(title = "Fit Results", y="Mean Value (fm)")

gMeans

#gDot <- ggplot(dataTrim, aes(x=LA010)) + geom_dotplot()
#gDot <- gDot + aes(x=LA1030)
#gDot

PlotMeasurements <- function(data, valColName) {
    
    # Get the columns indices associated with valColName and its error column
    errColName <- paste(valColName,"Err", sep = "")
    valColIndex <- which(colnames(data) == valColName)
    errColIndex <- which(colnames(data) == errColName)
    
    # Subset the data to take just the non-NA rows of those two columns
    dataset <- data[!is.na(data[,valColIndex]), c(valColName, errColName)]
    
    # Setup the ggplot of the data
    g <- ggplot(dataset, aes(y=valColName, x=as.numeric(rownames(dataset))))
    g <- g + geom_point(stat="identity", position = position_dodge(width=1))    
    gLimits <- aes(ymax = valColName + errColName, ymin = valColName - errColName)
    g <- g + geom_errorbar(gLimits)
    g
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