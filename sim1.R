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

# -*-*-*-*-* SIM1 -*-*-*-*-*

# ACF pour le choix de prccessus MA(q) à regarder par rapport au graphe de decroissance PACF. Ceci permet de vérifier
# l'hypothèse de stationarité supposé implicitement au debut. 
# PACF décroisance sinusoidal


# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
## Test des modele MA(1)
sim1.ma1 = arima(sim1, order = c(0,0,1), include.mean = TRUE)  # MA(1) c(2,0,0) AR(2)
summary(sim1.ma1)
# student
t_stat((sim1.ma1))

# test de portementeau
Box.test.2(sim1.ma1$residuals, nlag = c(5,10,15,20), type = "Ljung-Box", decim = 2)
# all p-value are greather than 0.005, donc on rejette pas H0. Rejette de l'hypotehse de blanceu

# sans la constante
sim1.a1_ = arima(sim1, order = c(0,0,1), include.mean = FALSE) 
summary(sim1.ma1_)


# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
## Test modele  MA(2)
sim1.ma2 = arima(sim1, order = c(0,0,2), include.mean = TRUE)  # MA(1) c(2,0,0) AR(2)
summary(sim1.ma2)
# student
t_stat((sim1.ma2))

## Test modele  AR(4)
sim1.ar4 = arima(sim1, order = c(4,0,0), include.mean = TRUE)  # MA(1) c(2,0,0) AR(2)
summary(sim1.ar4)
# student
t_stat((sim1.ar4))

# test de portementeau
Box.test.2(sim1.ar4$residuals, nlag = c(5,10,15,20), type = "Ljung-Box", decim = 2)


# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*



