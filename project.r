setwd("C:\\Users\\JCurt\\OneDrive\\MSc\\Sem 2\\EC6062 Applied Econometrics for Business\\Project")

library(readxl)
library(ggplot2)
library(dplyr)
library(GGally) # For Correlation
library(psych) # For describe
library(lmtest) # Hetero check
library(sandwich) # Hetero check

dataSet <- read_excel("DataPanelWHR2021C2.xlsx")
dataSet$year = factor(dataSet$year)
dataSet$`Country name` = factor(dataSet$`Country name`)

# Mean, median, etc
summary(subset(dataSet, select = -c(`Country name`, year, `Log GDP per capita`)))

#Removing the outliers with a GDP of 1
dataSet <- subset(dataSet, dataSet$`GDP per capita` != 1)
  

# plot of Life Ladder against year
gd <- dataSet %>% 
  group_by(year) %>% 
  summarise(
    Happiness = mean(Happiness))

ggplot(gd, aes(x = year,y = Happiness, color = year, group = 1)) +
  geom_point() + geom_line() + theme_minimal() +
  ggtitle("Mean Happiness Against Year") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold"))


# plot of GDP against year
gd <- dataSet %>% 
  group_by(year) %>% 
  summarise(
    GDP = mean(exp(`Log GDP per capita`)))

ggplot(gd, aes(x = year,y = GDP, color = year, group = 1)) +
  geom_point() + geom_line() + theme_minimal() + ylab("GDP per Capita") +
  ggtitle("Mean GDP per Capita Against Year") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold"))



# Summarizing the mean of each value for each year
summaryDataSet <- aggregate(. ~ year, data = dataSet,
                            function(x) c(mean = mean(x, na.rm=TRUE)), na.action = na.pass)
summaryDataSet <- subset(summaryDataSet, select = -c(`Country name`))
summaryDataSet



# Correlation Plot  #https://stackoverflow.com/questions/31412514/na-values-not-being-excluded-in-cor
ggpairs(subset(dataSet, select = -c(`Country name`,`GDP per capita`,year)), use="complete.obs")  +
  theme_void()


# Creating linear models

summary(lm(Happiness ~ `Log GDP per capita`, dataSet))
hModel <- lm(Happiness ~ `Log GDP per capita` + `Healthy life expectancy at birth`+ `Social support`, dataSet)

summary(hModel)

coeftest(hModel, vcov = vcovHC(hModel, "HC1"))


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(hModel)


ggplotRegression(hModel)

# plot of linear model
# modelData <- read_excel("model.xlsx")
# modelData$year = factor(modelData$year)
# 
# 
# ggplot(modelData, aes(x=year)) + 
#   geom_line(aes(y = Actual, color = "Actual"), group = 1, size = 1) + 
#   geom_line(aes(y = Predicted, color = "Predicted"), group = 1, size = 1) +
#   theme_minimal() +
#   ylab("Happiness") +
#   ggtitle("Actual vs Predicted Mean Happiness Against Year") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) +
#   scale_color_manual(name = "Legend", values = c("Actual" = "darkred", "Predicted" = "steelblue")) +
#   theme(legend.text = element_text(size = 12)) +
#   theme(legend.title = element_text(size = 14)) +
#   theme(axis.title.y = element_text(size = 12)) +
#   theme(axis.title.x = element_text(size = 12))


modelData <- read_excel("Cool Model.xlsx")


ggplot(modelData, aes(x=`GDP per capita`)) + 
  geom_point(aes(y = Actual, color = "Actual"), group = 1, size = 1) +
  geom_point(aes(y = Predicted, color = "Predicted"), group = 1, size = 1) +
  theme_minimal() +
  ylab("Happiness") +
  ggtitle("Actual vs Predicted Mean Happiness Against GDP per Capita") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) +
  scale_color_manual(name = "Legend", values = c("Actual" = "darkred", "Predicted" = "steelblue")) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12))


ggplot(modelData, aes(x=`Log GDP per Capita`)) + 
  geom_point(aes(y = Actual, color = "Actual"), group = 1, size = 1) +
  geom_point(aes(y = Predicted, color = "Predicted"), group = 1, size = 1) +
  theme_minimal() +
  ylab("Happiness") +
  ggtitle("Actual vs Predicted Mean Happiness Against Log GDP per Capita") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) +
  scale_color_manual(name = "Legend", values = c("Actual" = "darkred", "Predicted" = "steelblue")) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12))



# Plot of Irealnd Happiness against GDP
irelandData <- read_excel("Ireland Model.xlsx")


ggplot(irelandData, aes(x=`GDP per Capita`)) + 
  geom_line(aes(y = Actual, color = "Actual"), group = 1, size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), group = 1, size = 1) +
  theme_minimal() +
  ylab("Happiness") +
  ggtitle("Ireland's Actual vs Predicted Mean Happiness Against GDP per Capita") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) +
  scale_color_manual(name = "Legend", values = c("Actual" = "green4", "Predicted" = "orange3")) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12))  +
  ylim(6,8)

install.packages("MuMIn")
library(MuMIn)
combinations <- dredge(hModel)
library(car)
linearHypothesis(hModel, c("size=0", "expenditure=0"), white.adjust = "hc1")
