require(fma)
summary(forecast((HoltWinters(beer, seasonal = "multiplicative")),h=1))
?forecast
plot(forecast(HoltWinters(beer,seasonal="multiplicative")))
plot(forecast(HoltWinters(beer,beta=FALSE,gamma=FALSE)))
plot(forecast(HoltWinters(beer,gamma=FALSE)))
?HoltWinters

plot(forecast(HoltWinters(airpass)))
plot(forecast(HoltWinters(dole)))
plot(forecast(HoltWinters(dowjones, gamma=FALSE)))
















