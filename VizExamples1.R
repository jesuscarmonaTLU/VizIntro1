## VizExamples1.r
## This R File provides an introduction to Visualization
## Charts presented here are: 
## Line charts - Bar Charts - Scatter Plots - Box Plots - Histograms - Heat maps

#adding libraries
library(forecast)
library(ggplot2)
library(gplots)
library(reshape) 

#line charts - used to layout time series
#use Amtrak dataset - information on ridership

amtrak.df <- read.csv("Amtrak.csv")

ridership.ts <- ts(amtrak.df$Ridership, start= c(1991,1), end= c(2004, 3), freq = 12)
plot(ridership.ts, xlab="Year", ylab="Ridership (in 000)", ylim= c(1300, 2300))

#bar charts are use for counting element in categorical variables
#use Boston Housing dataset

boston.df <- read.csv("BostonHousing.csv")

## barchart of CHAS vs. mean MEDV
# compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(boston.df$MEDV, by = list(boston.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV,  names.arg = data.for.plot$CHAS, 
        xlab = "CHAS", ylab = "Avg. MEDV")

# alternative plot with ggplot
ggplot(data.for.plot) + geom_bar(aes(x = CHAS, y = MeanMEDV), stat = "identity")

## barchart of CHAS vs. % CAT.MEDV
data.for.plot <- aggregate(boston.df$CAT..MEDV, by = list(boston.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanCATMEDV")
barplot(data.for.plot$MeanCATMEDV * 100,  names.arg = data.for.plot$CHAS, 
        xlab = "CHAS", ylab = "% of CAT.MEDV")

## scatter plot with axes names
plot(boston.df$MEDV ~ boston.df$LSTAT, xlab = "MDEV", ylab = "LSTAT")
# alternative plot with ggplot
ggplot(boston.df) + geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.7)


## histogram of MEDV
hist(boston.df$MEDV, xlab = "MEDV")
# alternative plot with ggplot
ggplot(boston.df) + geom_histogram(aes(x = MEDV), binwidth = 5)

## boxplot of MEDV for different values of CHAS
boxplot(boston.df$MEDV ~ boston.df$CHAS, xlab = "CHAS", ylab = "MEDV")
# alternative plot with ggplot
ggplot(boston.df) + geom_boxplot(aes(x = as.factor(CHAS), y = MEDV)) + xlab("CHAS")

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(boston.df$NOX ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
boxplot(boston.df$LSTAT ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
boxplot(boston.df$PTRATIO ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
boxplot(boston.df$INDUS ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")

## simple heatmap of correlations (without values)
heatmap(cor(boston.df), Rowv = NA, Colv = NA)

## heatmap with values
heatmap.2(cor(boston.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(boston.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# to generate input for the plot
cor.mat <- round(cor(boston.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))
