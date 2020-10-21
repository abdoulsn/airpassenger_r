library(caschrono)
library(forecast)

# Load data dans simX
sim1 <- read.table("../../data_in/sim1.txt", header=FALSE, col.names="Elec", comment.char=",")
sim2 <- read.table("../../data_in/sim2.txt", header=FALSE, col.names="Elec", comment.char=",")

# convertir en time serie
sim1ts <- ts(sim1)
sim2ts <- ts(sim2)

# plot sim1
par(mfrow=c(2,2))
plot(sim1ts)
nlag = 50
# avec limit ylim
plot(acf(sim1ts, lag.max = nlag, plot = FALSE), ylim = c(-1,1)) # Roh(0)=1 donc pas de sens de significativité

# plot sim1
plot(sim2ts)
nlag = 50
# avec limit ylim
plot(acf(sim2ts, lag.max = nlag, plot = FALSE), ylim = c(-1,1)) # Roh(0)=1 donc pas de sens de significativité

### SIM1

# ACF pour le choix de prccessus MA(q) à regarder par rapport au graphe de decroissance PACF. Ceci permet de vérifier
# l'hypothèse de stationarité supposé implicitement au debut. 

# PACF décroisance sinusoidal

## TEST des modeles supposé

sim1.arima = arima(sim1, order = c(0,0,1), include.mean = TRUE)
summary(sim1.arima)
t_stat((sim1.arima))

Box.test.2(sim1.arima$residuals, nlag = c(5,10,15,20), type = "Ljung-Box", decim = 5)