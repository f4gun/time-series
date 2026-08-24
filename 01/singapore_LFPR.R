getwd()
setwd("C:/Users/lenovo/Desktop/learn/R/")
getwd()

# Data for Singapore (Credits: gapminder.org)
# Labour Participation Rate % (15-64)

singapore <- c(
  65.383, 65.91, 66.392, 66.805, 67.342, 68.01, 68.497, 68.996, 
  69.365, 69.566, 69.797, 69.981, 69.19, 69.381, 69.457, 70.199, 
  70.961, 70.228, 71.282, 71.153, 72.02, 71.846, 72.48, 72.336, 
  73.301, 74.548, 74.413, 74.361, 74.119, 74.749, 74.58, 76.847, 
  76.092, 76.097, 76.882, 77.627, 78.344
)
singapore= ts(singapore, start=1980)
plot(singapore, ylab= "Labour Force Participation Rate in % Aged 15-64")

# Exponential smoothing with holt

library(forecast)
holttrend = holt(singapur, h = 5)
summary(holttrend)
plot(holttrend)
plot(holt(singapore, h = 5, damped = T, phi = 0.8)) # Damped Holt trend

# Arima auto generated 
singaporearima= auto.arima(singapore)
summary(singaporearima)
plot(forecast(singaporearima, h = 5))

# Exact calculation of Arima parameters 
auto.arima(singapore, stepwise=F, approximation=F)

# Comparision of the models 
holttrend=holt(singapore,h=10)
holtdamped= holt(singapore, h=10, damped= T)
arimafore= forecast(auto.arima(singapore), h=10)

library(ggplot2)
autoplot(singapore) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("year") + ylab("Labour Force Participation Rate Age 15-64") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Singapore") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                      face="bold", size=15))

