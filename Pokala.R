# Assignment -1
# Siva Nanda Reddy POkala
# Number : 1509135

install.packages("tidyverse")
install.packages("tsibble")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("tseries")
install.packages("forecast")
install.packages("feasts")
install.packages("fable")
install.packages("Quandl")
install.packages("moments")
install.packages("gridExtra")



# libraries used
library(gridExtra)
library(moments)
library(ggplot2)
library(feasts)
library(fable)
library(Quandl)
library(dplyr)
library(tsibble)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(forecast)


Quandl.api_key("pt-WqgA2gRUFEyvsrtYV")

# Question 1


# Get the data using Quandl: We need weekly historical data ("xts")
# gold_data <- Quandl("LBMA/GOLD", type="xts", collapse="weekly")


# in this analysis i am considering in USD as base currency,first two columns are loading into data .
data <- Quandl(c("LBMA/GOLD.1", "LBMA/GOLD.2"), type="xts", collapse="weekly")

# Printing the firts 5 rows of the data 
head(data,5)

# Here i am removing the  na values from the dataset
data_mod <- na.omit(data)

# Renaming the column names as USD_AM(open) and USD_PM (close )
colnames(data_mod) <- c("USD_AM" , "USD_PM")

summary(data_mod)





# Question 2:


# convenience of multiplication of 100 : Percentage change of log return
# creating a dataframe with 3 columns 1)day 2)price(USD_PM i.e close) 3)log_return

final_data <- data.frame(
  day = as.Date(rownames(data.frame(data_mod))),
  price = as.numeric(data_mod$USD_PM),
  log_return = diff(log(data_mod$USD_PM) * 100 , lag = 1) 
)
# considered log return for lag=1, if we multiply by 100, we get percentage change


# change the third column name
names(final_data)[3] <- "log_return"

head(final_data)




# Question 3


# ploting the price  varible day on x-axis and price on y-axis

plot <- final_data %>%
  ggplot(aes(x=day, y=price)) +
  geom_line(color="blue", size = 1) + 
  xlab("day") +
  ylab("price") +
  ggtitle("Price plot") +
  theme_bw() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y")
plot


# ploting the log_return varible day on x-axis and log return on y-axis
plot <- final_data %>%
  ggplot(aes(x=day, y=log_return)) +
  geom_line(color="blue", size = 1) + 
  xlab("day") +
  ylab("log return") +
  ggtitle("log return plot") +
  theme_bw() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y")
plot


# test for normality 

data_check <- data.frame(
  day = as.Date(rownames(data.frame(data_mod))),
  return = c(NA, 100 * diff(data_mod$USD_PM)/
               data_mod$USD_PM[-length(data_mod$USD_PM)]) 
)

return <- na.remove(data_check[,"return"])
log_return <- na.remove(final_data[,"log_return"])

# returns are negatively skewed
#return has skewness -> (-0.1290003)
#log-return has skewness -> 0.3622819 

skewness(return)  
skewness(log_return) 

#kurtosis is  > 3 : not normally distributed
#return has kurtosis     -> 13.26525
#log-return has kurtosis -> 14.49796
kurtosis(return)
kurtosis(log_return)

# Null hypothesis : Normality
# Reject null hypothesis of normality because p value is very small 
# Even the log_return is not normally distributed , p value is very small

jarque.bera.test(na.remove(final_data[,"log_return"]))

jarque.bera.test(na.remove(data_check[,"return"])) 

# plotting the Histrogram

histogram <- hist(return, breaks = 120, col = "deeppink", 
                  xlab = "Return", main ="Histogram of LBMA/GOLD Returns", 
                  xlim = c(-10,10)) 

histogram <- hist(log_return, breaks = 120, col = "green", 
          xlab = "Return", main ="Histogram of LBMA/GOLD log Returns", 
          xlim = c(-10,10)) 





# Question 4:


# Inconveniences of using weekly time series are as stated below: 
# Weekly data can't handle holidays and their lead/lag relationships
# As the number of weeks in a year is subject to change, it creates statistical analysis issues

# Remove the na values
final_data <- na.omit(final_data)

# The day when the price/log return was the highest
# "2020-08-09"
price_high <- filter(final_data, (price == max(price))) 
price_high$day

#"1980-01-20"
log_return_high <- filter(final_data, log_return == max(log_return)) 
log_return_high$day


# The day when the price/log returnwas the lowest
# "1970-01-18"
price_low <- filter(final_data, (price == min(price))) 
price_low$day

#"1980-01-27"
log_return_low <- filter(final_data, log_return == min(log_return)) 
log_return_low$day




# Question 5


# Create data frame of xts weekly gold data 
# caution while converting weekly to monthly: April, 1968 had 4 weeks, June, 1968 had 5 weeks
# So we can't generalise for loop 

# time variable(year_month) column is added to existing data frame  
final_data <- mutate(final_data, Year_month = yearmonth(day))

# Group the data based on Year_month column and take the last observation of a particular year_month
# Alternatively we can take the first observation of a particular year_month
final_data_monthly <- final_data %>% group_by(Year_month) %>% slice(c(n()))


#tsibble object
final_data_monthly <- final_data_monthly %>%
  as_tsibble(index = Year_month)

# Remove missing value 
final_data_monthly <- na.omit(final_data_monthly)




# Question 6:


# No autocorrelation exist in the monthly log return
log_return_monthly <- na.remove(final_data_monthly[,"log_return"])

acf_check <- log_return_monthly %>%
  acf(lag = 120)                  
plot(acf_check[1:360], main= "ACF") 




# Question 7


# Price variable has upward trend
# we cannot find any trend in log return

# Plots 
plot <- ggplot(final_data_monthly, aes(x=day, y=price)) +
  geom_line(color="deeppink") + 
  xlab("Date") +
  ylab("price") +
  theme_bw() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")
plot 

plot <- ggplot(final_data_monthly, aes(x=day, y=log_return)) +
  geom_line(color="cyan2") + 
  xlab("Day") +
  ylab("log_return") +
  theme_bw() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")
plot 





# Question 8


# lb_pvalue for arma44 is 98.7%
# which is greater than significance level 5%
# so We are accepting the Null hypothesis which means  there is no auto correlation.
# possible combinations of AR(p) and MA(q) orders implemented in below---->ARIMA(0,0,0) - ARIMA(4,0,4)

arima <- final_data_monthly %>% 
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )



# 4th lag has minimum AIC (arma44) 
# 0th lag has minimum BIC (arma00)
glance(arima)
glance(arima)[which.min(glance(arima)[["AIC"]]), ] 
glance(arima)[which.min(glance(arima)[["BIC"]]), ]  


# Question 9

#Summary
arima %>%
  select(arma44) %>%
  report() 

a <- arima %>%
  residuals() %>% # get the residuals
  group_by(.model) %>% # groups the residuals by model
  features(features = ljung_box, lag = 20) # Ljung-Box test for autocorrelation with 20 lags

# No auto correlation
# lb_pvalue  is higher i.e 0.986 so we are accepting null hypothesis which means there is no autocorrelation
a[25,]




#Question 10



# ARIMA model (arma00) and NAive forecast stays at the same level. 
# from the results we can conclude our model works better the naive model
# model has less value for MAE,RMSE,MAE,..lower value is better



# data for previous months i.e 1968-04-28 to 2020-04-26

data_Y2020 <- final_data_monthly[1:625,]

# Creating a Model 
arima_Y2020 <- data_Y2020 %>% # possible combinations of AR(p) and MA(q) orders
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )


glance(arima_Y2020)
glance(arima_Y2020)[which.min(glance(arima_Y2020)[["AIC"]]), ] # arma34
glance(arima_Y2020)[which.min(glance(arima_Y2020)[["BIC"]]), ] # arma00

# Considering BIC as information criteria at secelted arma00 for forecasting
forecast12 <- data_Y2020 %>%
  model(ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0))) %>%
  forecast(h = 13, level = 95) # future prediction of a TS from the fitted model, 13 periods ahead

#accuracy for model vs actual data
accuracy(forecast12, final_data_monthly) 


# Naive forecast
forecast_naive <- data_Y2020 %>%
  model(NAIVE(log_return)) %>%
  forecast(h = 13)

#accuracy for naive vs actual data
accuracy(forecast_naive, final_data_monthly)

#model forecast
p1 <- autoplot(forecast12, final_data_monthly[600:638, ]) + 
  xlab("") + ylab("ARIMA(0,0,0)")

plot(p1)

#naive forecast
p2 <- autoplot(forecast_naive,final_data_monthly[600:638, ]) +
  xlab("") + ylab("Naive")

p <- grid.arrange(p1, p2, ncol = 1, nrow = 2) 
# combine the two plots in one
plot(p)


# Bonus Questions


# Question 1

set.seed(10)
x<-rnorm(5000,5,1)
x

# Question 2

logliknormal<- function(X,mu,sigma) {
  
  -sum(dnorm(X,mean=mu,sd=sigma,log=TRUE))
  
}

logliknormal(c(5,6,7),3,1)


# Question 3

mean_hat <- seq(0,10,0.02)
mean_hat
     