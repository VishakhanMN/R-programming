install.packages('pacman')
library(pacman)
pacman::p_load(pacman,rio,tidyverse)
  
educationStats <- import('Education Statistics.csv')
educationStats

## ----------------------------PRE PROCESSING MISSING VALUES WITH MEAN----------

##ARGENTINA

argentinaEduStats <- subset(educationStats, educationStats$`Country Code` == 'ARG')
argentinaEduStats

#CHECK FOR NULL VALUE
is.na(argentinaEduStats$`Net flow - international students`)

#REPLACE WITH MEAN VALUE
argentinaEduStats$`Net flow - international students`[is.na(argentinaEduStats$`Net flow - international students`)] <- mean(argentinaEduStats$`Net flow - international students`, na.rm =  TRUE)
argentinaEduStats


##BRAZIL

brazilEducationStats <- subset(educationStats, educationStats$`Country Code` == 'BRA')

is.na(brazilEducationStats$`GER - tertiary education`)
is.na(brazilEducationStats$`GER - Primary`)
is.na(brazilEducationStats$`GER - tertiary -Male`)
is.na(brazilEducationStats$`Net flow - international students`)

brazilEducationStats$`GER - tertiary education`[is.na(brazilEducationStats$`GER - tertiary education`)] <-mean(brazilEducationStats$`GER - tertiary education`, na.rm = TRUE)
brazilEducationStats$`GER - Primary`[is.na(brazilEducationStats$`GER - Primary`)] <- mean(brazilEducationStats$`GER - Primary`, na.rm = TRUE)
brazilEducationStats$`GER - tertiary -Male`[is.na(brazilEducationStats$`GER - tertiary -Male`)] <- mean(brazilEducationStats$`GER - tertiary -Male`, na.rm =  TRUE)
brazilEducationStats$`Net flow - international students`[is.na(brazilEducationStats$`Net flow - international students`)] <- mean(brazilEducationStats$`Net flow - international students`, na.rm= TRUE)
brazilEducationStats

##CHILE

chileEduStats <-subset(educationStats, educationStats$`Country Code` == 'CHL' | educationStats$`Country Code` == 'CHL')
chileEduStats

##MEXICO
mexicoEduStats <- subset(educationStats, educationStats$`Country Code` == 'MEX')

mexicoEduStats$`Govt Expenditure (USD)`[is.na(mexicoEduStats$`Govt Expenditure (USD)`)] <-mean(mexicoEduStats$`Govt Expenditure (USD)`,na.rm = TRUE)
mexicoEduStats$`Percentage with no schooling - adults`[is.na(mexicoEduStats$`Percentage with no schooling - adults`)] <-mean(mexicoEduStats$`Percentage with no schooling - adults`,na.rm = TRUE)
mexicoEduStats

##URUGUAY
uruguayEduStats <- subset(educationStats, educationStats$`Country Code` == 'URY')
uruguayEduStats

##CHINA
chinaEduStats <- subset(educationStats, educationStats$`Country Code` == 'CHN')
chinaEduStats

##JAPAN
japanEduStats <- subset(educationStats, educationStats$`Country Code` == 'JPN')

japanEduStats$`Govt Expenditure (USD)`[is.na(japanEduStats$`Govt Expenditure (USD)`)] <-mean(japanEduStats$`Govt Expenditure (USD)`,na.rm = TRUE)
japanEduStats

##MALAYSIA
malaysiaEduStats <- subset(educationStats, educationStats$`Country Code` == 'MYS')

malaysiaEduStats$`GER - Primary`[is.na(malaysiaEduStats$`GER - Primary`)] <-mean(malaysiaEduStats$`GER - Primary`, na.rm = TRUE)
malaysiaEduStats

##SINGAPORE
singaporeEduStats <- subset(educationStats, educationStats$`Country Code` == 'SGP')

singaporeEduStats$`Net enrollment rate` [is.na(singaporeEduStats$`Net enrollment rate`)] <-mean(singaporeEduStats$`Net enrollment rate`,na.rm =  TRUE)
singaporeEduStats

##THAILAND
thailandEduStats <- subset(educationStats, educationStats$`Country Code` == 'THA')
thailandEduStats

##FRANCE
franceEduStats <- subset(educationStats, educationStats$`Country Code` == 'FRA')
franceEduStats


##GERMANY
germanEduStats <- subset(educationStats, educationStats$`Country Code` == 'DEU')

germanEduStats$`Govt Expenditure (USD)`[is.na(germanEduStats$`Govt Expenditure (USD)`)] <-mean(germanEduStats$`Govt Expenditure (USD)`,na.rm = TRUE)
germanEduStats


##SPAIN
spainEduStats <- subset(educationStats, educationStats$`Country Code` == 'ESP')

spainEduStats$`Govt Expenditure (USD)`[is.na(spainEduStats$`Govt Expenditure (USD)`)] <-mean(spainEduStats$`Govt Expenditure (USD)`,na.rm = TRUE)
spainEduStats$`Youth literacy rate`[is.na(spainEduStats$`Youth literacy rate`)] <- mean(spainEduStats$`Youth literacy rate`, na.rm =  TRUE)
spainEduStats

##SWITZERLAND
switzEduStats <- subset(educationStats, educationStats$`Country Code` == 'CHE')
switzEduStats$`Govt Expenditure (USD)`[is.na(switzEduStats$`Govt Expenditure (USD)`)] <-mean(switzEduStats$`Govt Expenditure (USD)`,na.rm = TRUE)
switzEduStats

##UK 
ukEduStats <- subset(educationStats, educationStats$`Country Code` == 'GBR')
ukEduStats$`Net enrollment rate - upper secondary`[is.na(ukEduStats$`Net enrollment rate - upper secondary`)] <-mean(ukEduStats$`Net enrollment rate - upper secondary`, na.rm =  TRUE)
ukEduStats

##-----------------------------------------------------DONE---------------------

##-------------------CONTINENT CLASSIFICATION-----------------------------------

europeanEduStats <- rbind(ukEduStats,switzEduStats,spainEduStats,germanEduStats,franceEduStats)
asianEduStats <- rbind(chinaEduStats,japanEduStats,malaysiaEduStats,thailandEduStats,singaporeEduStats)
latinEduStats <- rbind(uruguayEduStats,mexicoEduStats,brazilEducationStats,argentinaEduStats,chileEduStats)

##-----------------------COMBINING ALL------------------------------------------

finalEduStats <- rbind(europeanEduStats,asianEduStats,latinEduStats)
finalEduStats

##-----------------------PART 1-------------------------------------------------

summary(europeanEduStats$GDP)
summary(asianEduStats)
summary(latinEduStats)

##----------------------MEAN----------------------------------------------------
mean(europeanEduStats$`Govt Expenditure (USD)`, na.rm = TRUE)
mean(europeanEduStats$`Net flow - international students`)
mean(europeanEduStats$Unemployment)
mean(europeanEduStats$`Teachers - secondary education`)


mean(asianEduStats$`Govt Expenditure (USD)`, na.rm = TRUE)
mean(asianEduStats$`Net flow - international students`)
mean(asianEduStats$`Unemployment`)
mean(asianEduStats$`Teachers - secondary education`)


mean(latinEduStats$`Govt Expenditure (USD)`, na.rm = TRUE)
mean(latinEduStats$`Net flow - international students`)
mean(latinEduStats$`Unemployment`)
mean(latinEduStats$`Teachers - secondary education`)

mean(europeanEduStats$`Overall Labour Force`)
mean(asianEduStats$`Overall Labour Force`)
mean(latinEduStats$`Overall Labour Force`)

meanDataset <- c(mean(asianEduStats$`Overall Labour Force`),
                 mean(latinEduStats$`Overall Labour Force`),
                 mean(europeanEduStats$`Overall Labour Force`))

barplot(meanDataset, names.arg=c('Asia','South America','Europe'),
        col = c("red", "green", "blue"), main="Overall Labour Force",
        xlab="Continent")

meanDataset <- c(mean(asianEduStats$`Net flow - international students`),
                 mean(latinEduStats$`Net flow - international students`),
                 mean(europeanEduStats$`Net flow - international students`))

barplot(meanDataset, names.arg=c('Asia','South America','Europe'),
        col = c("red", "green", "blue"),
        main="Net inflow of international Students",
        xlab="Continent")

##---------------------MEDIAN---------------------------------------------------

median(europeanEduStats$`Unemployment`)
median(europeanEduStats$`Net flow - international students`)
median(europeanEduStats$`Overall Labour Force`)

median(asianEduStats$`Unemployment`)
median(asianEduStats$`Net flow - international students`)
median(asianEduStats$`Overall Labour Force`)

median(latinEduStats$`Unemployment`)
median(latinEduStats$`Net flow - international students`)
median(latinEduStats$`Overall Labour Force`)


median(asianEduStats$`Youth literacy rate`)
median(latinEduStats$`Youth literacy rate`)
median(europeanEduStats$`Youth literacy rate`)

medianDataset <- c(median(asianEduStats$`Youth literacy rate`),
                   median(latinEduStats$`Youth literacy rate`),
                   median(europeanEduStats$`Youth literacy rate`))

barplot(medianDataset, names.arg=c('Asia','South America','Europe'), 
        main="Youth Literacy", col = c("red", "green", "blue"),
        xlab="Continent")


##----------------------MODE----------------------------------------------------

# Create the function.
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getMode(europeanEduStats$`Compulsory years - legal`)
getMode(europeanEduStats$`Overall Labour Force`)
getMode(europeanEduStats$`Teachers - secondary education`)


getMode(asianEduStats$`Compulsory years - legal`)
getMode(asianEduStats$`Overall Labour Force`)
getMode(asianEduStats$`Teachers - secondary education`)

getMode(latinEduStats$`Compulsory years - legal`)
getMode(latinEduStats$`Overall Labour Force`)
getMode(latinEduStats$`Teachers - secondary education`)

getMode(asianEduStats$`Govt Expenditure (USD)`)
getMode(latinEduStats$`Govt Expenditure (USD)`)
getMode(europeanEduStats$`Govt Expenditure (USD)`)

modeDataset <- c(getMode(asianEduStats$`Govt Expenditure (USD)`),
                 getMode(latinEduStats$`Govt Expenditure (USD)`),
                 getMode(europeanEduStats$`Govt Expenditure (USD)`))

barplot(modeDataset, names.arg=c('Asia','South America','Europe'),
        main="Government expenditure", col = c("red", "green", "blue"),
        xlab="Continent")

##-------------------STANDARD DEVIATION-----------------------------------------

sd(europeanEduStats$`Unemployment`)
sd(europeanEduStats$`Net flow - international students`)
sd(europeanEduStats$`Overall Labour Force`)

sd(asianEduStats$`Unemployment`)
sd(asianEduStats$`Net flow - international students`)
sd(asianEduStats$`Overall Labour Force`)

sd(latinEduStats$`Unemployment`)
sd(latinEduStats$`Net flow - international students`)
sd(latinEduStats$`Overall Labour Force`)

sd(asianEduStats$`Govt Expenditure (USD)`)
sd(latinEduStats$`Govt Expenditure (USD)`)
sd(europeanEduStats$`Govt Expenditure (USD)`)
  

sdDataset <- c(sd(asianEduStats$`Govt Expenditure (USD)`),
               sd(latinEduStats$`Govt Expenditure (USD)`),
               sd(europeanEduStats$`Govt Expenditure (USD)`))

barplot(sdDataset, names.arg=c('Asia','South America','Europe'),
        main="Standard Deviation - Government expenditure",
        col = c("red", "green", "blue"),
        xlab="Continent")

##--------------------KURTOSIS--------------------------------------------------

install.packages('moments')
library(moments)

kurtosis(asianEduStats$`Overall Labour Force`)
kurtosis(latinEduStats$`Overall Labour Force`)
kurtosis(europeanEduStats$`Overall Labour Force`)

kurtosis(europeanEduStats$`Teachers - secondary education`)
kurtosis(europeanEduStats$`Unemployment`)
kurtosis(europeanEduStats$`Net flow - international students`)


kurtosis(asianEduStats$`Teachers - secondary education`)
kurtosis(asianEduStats$`Unemployment`)
kurtosis(asianEduStats$`Net flow - international students`)

kurtosis(latinEduStats$`Teachers - secondary education`)
kurtosis(latinEduStats$`Unemployment`)
kurtosis(latinEduStats$`Net flow - international students`)

kurtosisDataset <- c(kurtosis(asianEduStats$`Overall Labour Force`),
                     kurtosis(latinEduStats$`Overall Labour Force`),
                     kurtosis(europeanEduStats$`Overall Labour Force`))

barplot(kurtosisDataset, names.arg=c('Asia','South America','Europe'),
        main="Kurtosis - `Overall Labour Force`",
        col = c("red", "green", "blue"),
        xlab="Continent")

##-------------------------SKEWNESS---------------------------------------------

skewness(europeanEduStats$`Teachers - secondary education`)
skewness(europeanEduStats$`Unemployment`)
skewness(europeanEduStats$`Net flow - international students`)


skewness(asianEduStats$`Teachers - secondary education`)
skewness(asianEduStats$`Unemployment`)
skewness(asianEduStats$`Net flow - international students`)

skewness(latinEduStats$`Teachers - secondary education`)
skewness(latinEduStats$`Unemployment`)
skewness(latinEduStats$`Net flow - international students`)

skewness(asianEduStats$`Teachers - secondary education`)
skewness(latinEduStats$`Teachers - secondary education`)
skewness(europeanEduStats$`Teachers - secondary education`)

skewnessDataset <- c(skewness(asianEduStats$`Teachers - secondary education`),
                     skewness(latinEduStats$`Teachers - secondary education`),
                     skewness(europeanEduStats$`Teachers - secondary education`))

barplot(skewnessDataset, names.arg=c('Asia','South America','Europe'),
        main="Skewness - `Teachers in secondary education`",
        col = c("red", "green", "blue"),
        xlab="Continent")

##-------------------CORRELATION ANALYSIS---------------------------------------

install.packages('corrplot')
install.packages('dplyr')

library(dplyr)
library(corrplot)


corrAsian <-subset(asianEduStats, select = c('GDP','Govt Expenditure (USD)',
                                             'GER - tertiary education','GER - Primary',
                                             'Overall Labour Force','Net flow - international students',
                                             'Labor force - advanced education','Labor force - basic education',
                                             'Percentage with no schooling - adults',
                                             'Youth literacy rate','Unemployment','Teachers - secondary education'))
corrAsian

corrLatin <-subset(latinEduStats, select = c('GDP','Govt Expenditure (USD)',
                                             'GER - tertiary education','GER - Primary',
                                             'Overall Labour Force','Net flow - international students',
                                             'Labor force - advanced education','Labor force - basic education',
                                             'Percentage with no schooling - adults',
                                             'Youth literacy rate','Unemployment','Teachers - secondary education'))
corrLatin

corrEurope <-subset(europeanEduStats, select = c('GDP','Govt Expenditure (USD)',
                                             'GER - tertiary education','GER - Primary',
                                             'Overall Labour Force','Net flow - international students',
                                             'Labor force - advanced education','Labor force - basic education',
                                             'Percentage with no schooling - adults',
                                             'Youth literacy rate','Unemployment','Teachers - secondary education'))
corrEurope


corrplot(cor(corrAsian), method = "number", type = "upper")
corrplot(cor(corrLatin), method = "number", type = "upper")
corrplot(cor(corrEurope), method = "number", type = "upper")


##------------------------------REGRESSION ANALYSIS-----------------------------

install.packages('caret')
install.packages('car')
library(caret)
library(car)


##---------------------Regression analysis for Asian Countries------------------

corrplot(cor(corrAsian), method = "number", type = "upper")


phaseOneModel <-lm(`GDP` ~ `Govt Expenditure (USD)`, corrAsian)
summary.lm(phaseOneModel)
## Multiple R-squared:  0.1597,	Adjusted R-squared:  0.1438  3* and 2*

##    Add multiple variables
phaseTwoModel <-lm(`GDP` ~  `Govt Expenditure (USD)` +
                       `Net flow - international students`, corrAsian)
summary.lm(phaseTwoModel)
##  Multiple R-squared:  0.2786,	Adjusted R-squared:  0.2509 3*,1*, 2*

phaseThreeModel <- lm(`GDP` ~  `Govt Expenditure (USD)` +
                       `Net flow - international students` +
                       `Youth literacy rate`, corrAsian)
summary.lm(phaseThreeModel)
##  Multiple R-squared:  0.5836,	Adjusted R-squared:  0.5591 all 3*

phaseFourModel <- lm(`GDP` ~ `Youth literacy rate` + `Govt Expenditure (USD)` +
                       `Net flow - international students`  +
                       `Labor force - advanced education`, corrAsian)
summary.lm(phaseFourModel)
##  Multiple R-squared:  0.7288,	Adjusted R-squared:  0.7071  all variables have 3* 


vif(phaseFourModel)

##-----------------------------------CHECK ASSUMPTIONS--------------------------
corrAsian

#  Linearity check
pairs(corrAsian[,c(1,10,2,6,7)], lower.panel = NULL, pch = 19,cex = 0.2)

##  Residual independance
plot(phaseFourModel,1)

###  Normality of residuals
plot(phaseFourModel, 2)

####   Homoscedasticity
plot(phaseFourModel, 3)


#####  vif
vif(phaseFourModel)

##---------------------------------TIME SERIES ANALYSIS-------------------------

install.packages('TTR')
install.packages("forecast")
install.packages('zoo')

library(TTR)
library(forecast)
library(zoo)

png(file ="timeSeries.png") # output to be a png file

meanUnemploymentDataset <- c(mean(asianEduStats$Unemployment),
                             mean(latinEduStats$Unemployment),
                             mean(europeanEduStats$Unemployment))

barplot(meanUnemploymentDataset, names.arg=c('Asia','South America','Europe'),
        col = c("red", "green", "blue"), main="Unemployment Stats",
        xlab="Continent")

##---------------ASIAN----------------------------------------------------------

ggplot(asianEduStats, aes(x=Year, y=Unemployment, group=`Country Name`)) +
  geom_line(aes(color=`Country Name`))+
  geom_point(aes(color=`Country Name`))+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="top")

##---------------LATIN----------------------------------------------------------

ggplot(latinEduStats, aes(x=Year, y=Unemployment, group=`Country Name`)) +
  geom_line(aes(color=`Country Name`))+
  geom_point(aes(color=`Country Name`))+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="top")

##---------------EUROPE---------------------------------------------------------

ggplot(europeanEduStats, aes(x=Year, y=Unemployment, group=`Country Name`)) +
  geom_line(aes(color=`Country Name`))+
  geom_point(aes(color=`Country Name`))+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position="top")

##----------------COUNTRIES-----------------------------------------------------

# Create the function for line chart - Normal line chart.
plotChart <- function(country,mainValue) {
  unEmpTimeSeries <-ts(country$Unemployment,start = '2008', frequency = 1)
  plot(unEmpTimeSeries,xlab ="Yearly data",
       ylab ="Unemployment",
       main = mainValue,
       col.main ="darkgreen")
}

# Create the function for line chart -Simple moving average.
plotChartSMA <- function(country,mainValue) {
  unEmpTimeSeriesSMA <-ts(country$Unemployment,start = '2008', frequency = 1)
  plot(unEmpTimeSeriesSMA,xlab ="Yearly data",
       ylab ="Unemployment",
       main = mainValue,
       col.main ="darkgreen")
  lines(rollmean(unEmpTimeSeriesSMA,3),col='red')
}

# Create the function for line chart -Triangular moving average.
plotChartTMA <- function(country,mainValue) {
  unEmpTimeSeriesTMA <-ts(country$Unemployment,start = '2008', frequency = 1)
  plot(unEmpTimeSeriesTMA,xlab ="Yearly data",
       ylab ="Unemployment",
       main = mainValue,
       col.main ="darkgreen")
  lines(rollmean(rollmean(unEmpTimeSeriesTMA,3),3),col='blue')
}

# Create the function for forecasting.
plotForecastArima <- function(country,arimaCoefficient,mainValue){
  testCountry<- arima(country$Unemployment, order = arimaCoefficient)
  testCountryForecast<- forecast(testCountry,5)
  plot(testCountryForecast,xlab ="Year",
       ylab ="Unemployment",
       main = mainValue , col.main ="darkgreen")
}

#Test residuals
plotForecastArimaTestResidual <- function(country,arimaCoefficient,mainValue){
  testCountry<- arima(country$Unemployment, order = arimaCoefficient)
  testCountryForecast<- forecast(testCountry,5)
  acf(testCountryForecast$residuals, lag.max=10)
  Box.test(testCountryForecast$residuals, lag=9, type="Ljung-Box")
}

##------------------Holt Winters FORECAST---------------------------------------

holtWinterScaling <- function(country){
  holtWinterTestVariable <- ts(country$Unemployment, start = c(2008,1), frequency = 1)
  holtWinterTestVariableGraph <- HoltWinters(holtWinterTestVariable, gamma=FALSE)
  holtWinterTestVariableGraph 
  plot(holtWinterTestVariableGraph)
}


holtWinterForecast <- function(country){
  holtWinterTestVariableForecast <- ts(country$Unemployment, start = c(2008,1), frequency = 1)
  holtWinterTestVariableGraphFC <- HoltWinters(holtWinterTestVariableForecast, gamma=FALSE)
  holtWinterTestVariableGraphFC 
  forecastUnemploymentHW <- forecast:::forecast.HoltWinters(holtWinterTestVariableGraphFC, h=5)
  forecastUnemploymentHW
  plot(forecastUnemploymentHW)
}


##----------------FRANCE--------------------------------------------------------

plotChart(franceEduStats,"Unemployment stats in France")
plotChartSMA(franceEduStats,"Unemployment stats in France")
plotChartTMA(franceEduStats,"Unemployment stats in France")

auto.arima(franceEduStats$Unemployment)
plotForecastArima(franceEduStats, c(0,2,0),"Forecast Unemployment in France")
plotForecastArimaTestResidual(franceEduStats, c(0,2,0),"Forecast Unemployment in France")

holtWinterScaling(franceEduStats)
holtWinterForecast(franceEduStats)

##----------------GERMANY-------------------------------------------------------

plotChart(germanEduStats,"Unemployment stats in Germany")
plotChartSMA(germanEduStats,"Unemployment stats in Germany")
plotChartTMA(germanEduStats,"Unemployment stats in Germany")

auto.arima(germanEduStats$Unemployment)
plotForecastArima(germanEduStats, c(0,1,0),"Forecast Unemployment in Germany")
plotForecastArimaTestResidual(germanEduStats, c(0,1,0),"Forecast Unemployment in Germany")

holtWinterScaling(germanEduStats)
holtWinterForecast(germanEduStats)

##----------------SPAIN---------------------------------------------------------

plotChart(spainEduStats,"Unemployment stats in Spain")
plotChartSMA(spainEduStats,"Unemployment stats in Spain")
plotChartTMA(spainEduStats,"Unemployment stats in Spain")

auto.arima(spainEduStats$Unemployment)
plotForecastArima(spainEduStats, c(2,0,0),"Forecast Unemployment in Spain")
plotForecastArimaTestResidual(spainEduStats, c(0,1,0),"Forecast Unemployment in Spain")

holtWinterScaling(spainEduStats)
holtWinterForecast(spainEduStats)

##----------------UK------------------------------------------------------------

plotChart(ukEduStats,"Unemployment stats in UK")
plotChartSMA(ukEduStats,"Unemployment stats in UK")
plotChartTMA(ukEduStats,"Unemployment stats in UK")

auto.arima(ukEduStats$Unemployment)
plotForecastArima(ukEduStats, c(0,2,0),"Forecast Unemployment in UK")
plotForecastArimaTestResidual(ukEduStats, c(0,1,0),"Forecast Unemployment in UK")

holtWinterScaling(ukEduStats)
holtWinterForecast(ukEduStats)

##----------------SWITZERLAND---------------------------------------------------

plotChart(switzEduStats,"Unemployment stats in Switzerland")
plotChartSMA(switzEduStats,"Unemployment stats in Switzerland")
plotChartTMA(switzEduStats,"Unemployment stats in Switzerland")

auto.arima(switzEduStats$Unemployment)
plotForecastArima(switzEduStats, c(0,1,0),"Forecast Unemployment in Switzerland")
plotForecastArimaTestResidual(switzEduStats, c(0,1,0),"Forecast Unemployment in Switzerland")
plotForecastErrors(switzEduStats$Unemployment)

holtWinterScaling(switzEduStats)
holtWinterForecast(switzEduStats)


##----------------HYPOTHESIS TESTING--------------------------------------------

install.packages('RVAideMemoire')
library(RVAideMemoire)

boxplot(asianEduStats$GDP ~ asianEduStats$`Country Name` , data=asianEduStats,
        xlab="Country Name", ylab="GDP",
        main="GDP Of Asian Countries")

byf.shapiro(asianEduStats$GDP ~ asianEduStats$`Country Name`) #normality check

bartlett.test(asianEduStats$GDP ~ asianEduStats$`Country Name`,
              data=asianEduStats)

#False because 6th assumption is not there
oneway.test(asianEduStats$GDP ~ asianEduStats$`Country Name`,
            data=asianEduStats, var.equal = FALSE) 


##---------------------2nd Test-------------------------------------------------

meanUnemploymentDataset <- c(mean(asianEduStats$Unemployment),
                             mean(latinEduStats$Unemployment),
                             mean(europeanEduStats$DUnemployment))

barplot(meanUnemploymentDataset, names.arg=c('Asia','South America','Europe'),
        col = c("red", "green", "blue"), main="Unemployment stats",
        xlab="Continent")

boxplot(europeanEduStats$Unemployment ~ europeanEduStats$`Country Name` ,
        data=europeanEduStats,
        xlab="Country Name", ylab="Unemployment(%)",
        main="Unemployment in European countries")

byf.shapiro(europeanEduStats$Unemployment ~ europeanEduStats$`Country Name`) #normality check

bartlett.test(europeanEduStats$Unemployment ~ europeanEduStats$`Country Name`,
              data=europeanEduStats)

#False because 6th assumption is not there
oneway.test(europeanEduStats$Unemployment ~ europeanEduStats$`Country Name`,
            data=europeanEduStats, var.equal = FALSE) 

##--------------------COMPLETED-------------------------------------------------