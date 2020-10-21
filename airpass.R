
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
