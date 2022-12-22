##Import Packages

library(tidyverse)
library(dplyr)
library(zoo)
library(forecast)
library(dlookr)
library(TSstudio)
library(ggplot2)
library(readxl)
library(lubridate)
library(xts)
library(Hmisc)
library(rcompanion)
#import.packages('astsa')
install.packages('GGally')
library(astsa)
library(GGally)



##Import Data

fruitvege <- read_excel("TimeSeries/fruit_vege.xlsx")

head(fruitvege)
tail(fruitvege)

sum(is.null(fruitvege))  #checking for null values
str(fruitvege)


###Feature Engineer
#extract month and year from Date 
fruitvege <- fruitvege %>% mutate("Month"= format(fruitvege$Date,"%m"), 
                                    "Year"= format(fruitvege$Date,"%Y"))

## Descriptive stats from 2012-2021
summary(fruitvege[,c(2:9)])

## Descriptive stats per year
agg_mean <- aggregate(fruitvege[, c(2:9)],
                by = list(fruitvege$Year),
                FUN = mean)
agg_var <- aggregate(fruitvege[, c(2:9)],
                     by = list(fruitvege$Year),
                     FUN = var)

print(agg_mean)
print(agg_var)

##Plot converted timeseries of fruitvege
ts_frvg <- ts(fruitvege[, -c(1, 10:17)], start= c(2012, 1), end= c(2021, 12), 
              frequency = 12)

autoplot(ts_frvg, lwd = 1, alpha= 0.7)+
  labs(y="Price", x= "Date", title= "Prices of Fruit Vegetables vs Date")



## convert agg_mean to timeseries
aggmean_plot <- ts(agg_mean, start= c(2012), end = c(2021), frequency = 1)
plot(aggmean_plot[, c(2:9)], main = "Mean of Fruit Vegetables per Year",
         col= "blue", type= "b", lwd= 2)

###SESONAL PLOT
ggseasonplot(ts_frvg[,1], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
      ggtitle("Seasonal plot: Ampalaya per 1kg")

ggseasonplot(ts_frvg[,2], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Sayote per 1kg")

ggseasonplot(ts_frvg[,3], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Upo per 1kg")

ggseasonplot(ts_frvg[,4], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Pipino per 1kg")

ggseasonplot(ts_frvg[,5], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Talong per 1kg")

ggseasonplot(ts_frvg[,6], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Patola per 1kg")

ggseasonplot(ts_frvg[,7], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Squash per 1kg")

ggseasonplot(ts_frvg[,8], year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Prices") +
  ggtitle("Seasonal plot: Tomato per 1kg")

##Scatter Plot

autoplot(ts_frvg[,1:4], facets=TRUE) +
  ylab("Prices")

autoplot(ts_frvg[,5:8], facets=TRUE) +
  ylab("Prices")


##Correlation Matrices
#Correlation between fruit vegetables
GGally::ggpairs(as.data.frame(ts_frvg[,1:8]))

#Correlation between each fruitvege and events
ts_frvg_2 <- ts(fruitvege[, -1], start= c(2012, 1), end= c(2021, 12), 
              frequency = 12)  #<- ts with events

GGally::ggpairs(as.data.frame(ts_frvg_2[,c(1, 9:14)])) #ampalaya
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(2, 9:14)])) #sayote
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(3, 9:14)])) #Upo
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(4, 9:14)])) #Pipino
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(5, 9:14)])) #Talong
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(6, 9:14)])) #Patola
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(7, 9:14)])) #Squash
GGally::ggpairs(as.data.frame(ts_frvg_2[,c(8, 9:14)])) #Tomato



