suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forecast))
install.packages("astsa")
suppressPackageStartupMessages(library(astsa))
install.packages("Rmisc")
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))
library(tibble)
library(dplyr)



# reading diabetes related data 
df0 <- read.csv("health.csv", na.strings = c("","na"))
str(df0)
head(df0)
summary(df0)
df0

# creating the subset gender based

health_data <- subset (df, select=c(Sex, Year, Number.of.adults.with.diabetes ))

health_data

str(health_data)
summary(health_data)


# creating subsets for two different groups
Male <- subset(health_data,  Sex == "Men", select=c(Year, Number.of.adults.with.diabetes))
Male
str(Male)

Female <- subset(health_data,  Sex == "Women", select=c(Number.of.adults.with.diabetes))
Female
str(Female)

# getting side by side male and female diabetes rate
df <-cbind(Male, Female)
df

names(df)[2] <-"Male_diabetes_rate"
names(df)[3] <-"Female_diabetes_rate"

#Welch Two Sample t-test


t.test(Number.of.adults.with.diabetes ~ Sex, data = health_data)


# First step of time series analysis
# is to describe the data numerically and visually



# Plot the data first
# and smooth it to remove significant error components
# through centered moving average

ggplot(data = health_data, aes(x = Year)) + geom_line(aes(y = Number.of.adults.with.diabetes, colour = Sex)) +
  scale_colour_manual(values = c("blue", "red"))

install.packages("multiplot")

library(multiplot)

p1 <- ggplot(data = health_data, aes(x = Sex, y = Number.of.adults.with.diabetes)) + geom_boxplot()
p2 <- ggplot(data = df, aes(Male_diabetes_rate)) + geom_density()
p3 <- ggplot(data = df, aes(Female_diabetes_rate)) + geom_density()
plot(p1)
plot(p2)
plot(p3)

install.packages("basicStats")
library(ggplot2)
library(ggfortify)
autoplot(USAccDeaths)
# Let us define the time series to be analysed with frequency = 1 as data is collected yearly
excess_frac <- (df$Male_diabetes_rate - df$Female_diabetes_rat)/df$Female_diabetes_rat
excess_ts <- ts(excess_frac, frequency = 1, start = df$Year[1])
autoplot(excess_ts)


# basic stats metric calculated
basicStats(excess_frac)

urdftest_lag = floor(12*(length(excess_ts)/100)^0.25)
urdfTest(excess_ts, type = "nc", lags = urdftest_lag, doplot = FALSE)

urdfTest(excess_ts, type = "c", lags = urdftest_lag, doplot = FALSE)

# ACF and PACF plotting 
# Plotting the ACF chart - measure of how the observations
# in a time series relate to each other
# if the autocorrelation crosses the dashed blue line, it means
# specific lag is significantly correlated with current series
# A stationary time series will have autocorrelation fall quickly
# to 0, with non-stationary series it drops gradually

par(mfrow=c(1,2))
acf(excess_ts)

# partial autocorrelation plot
pacf(excess_ts)

summary(lm(excess_ts ~ 1))
(break_point <- breakpoints(excess_ts ~ 1))
plot(break_point)
summary(break_point)


plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))


# men vs women Diabetese ratio changed from 1980 to 2014

fitted(break_point)[1]

fitted(break_point)[length(excess_ts)]

# from 26.17407 to 66.4233

# ARIMA Model
# Fitting an ARIMA model 
# Note we use the original dataset for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
# We apply the model to the original time series
# non seasonal (1,1,1), as determined by auto.arima() within forecast package

(model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE))
summary(model_1)

# Accuracy measures
# The mean absolute percentage error (MAPE)
# measures prediction of accuracy
# Here the MAPE is 13% of the river level
# so this is the forecast accuracy of the error

accuracy(model_1)

# Evaluating model fit ---------------------------------------
# qqnorm produces a normal QQ plot of the values in y. 
# qqline adds a line to a “theoretical”, quantile-quantile plot 
# which passes through the probs quantiles, 
# by default the first and third quartiles
help("qqnorm")
qqnorm(model_1$residuals)
qqline(model_1$residuals)

coeftest(model_1)

# Box.test() function provides a test that autocorrelations 
# are all zero (H0). The results are significant, suggesting 
# the autocorrelations don ’t differ from zero.
# This ARIMA model appears to fit the data well.
Box.test(arima_model$residuals, type = "Ljung-Box")

# Residuals Diagnostic
checkresiduals(model_1)


LjungBoxTest(residuals(model_1), k = 2, lag.max = 20)

sarima(excess_ts, p = 1, d = 1, q = 1)

# forecasting 20 years a head

h_fut <- 20
plot(forecast(model_1, h = h_fut, xreg = rep(1, h_fut)))
