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

