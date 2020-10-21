
# plot sim1
par(mfrow=c(3,2),)
plot(AirPassengers) # --> Model multiplicatif, on passe à un model aditif par log
y = log(AirPassengers)
plot(y, title('log Airpass')) 

nlag = length(AirPassengers)*0.25
# avec limit ylim
plot(acf(y, lag.max = nlag, plot = FALSE), ylim = c(-1,1))
pacf(y, lag.max = nlag, ylim=c(-1,1))


# Premier diff de log(x)
base::diff(y,lag=1, differences=1) -> Y
plot(Y)
acf(Y, lag.max = nlag, ylim=c(-1,1))
pacf(Y, lag.max = nlag, ylim=c(-1,1))
# second diff de Y
base::diff(Y,lag=12, differences=1) -> Z
plot(Z)
acf(Z, lag.max = nlag, ylim=c(-1,1))
pacf(Z, lag.max = nlag, ylim=c(-1,1))

# Applying arima
airpass.arima = arima(y, order=c(3, 1, 2), seasonal = list(order = c(0, 1, 0), period = 12))
summary(airpass.arima)
# student
t_stat((airpass.arima))
# test de portementeau
Box.test.2(airpass.arima$residuals, nlag = c(5,10,15,20), type = "Ljung-Box", decim = 2)

# REGARDER LE RHO(CHAPO) POUR GRAND P(=1 une seul barre superieur), Q, D


# Applying 

acf(Z, lag.max = 11, ylim=c(-1,1))
pacf(Z, lag.max = 11, ylim=c(-1,1))

airpass.arima_2 = arima(y, order=c(1, 1, 2), seasonal = list(order = c(2, 1, 1), period = 12))
summary(airpass.arima_2)
# student
t_stat((airpass.arima_2))
# test de portementeau
Box.test.2(airpass.arima_2$residuals, nlag = c(5,10,15,20), type = "Ljung-Box", decim = 2)
