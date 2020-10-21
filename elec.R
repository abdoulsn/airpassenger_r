# Load data dans don
don <- read.table("../../data_in/elec.csv", header=FALSE, col.names="Elec", comment.char=",")
head(don)

# convertir en time serie
don_ts <- ts(don,start=c(1956,1),frequency=12)
don_ts

# stats basics
frequency(don_ts)

# stats trimestriels
ts(1:9,start=c(1990,3),frequency=4)

# instance windows
window(don_ts,start=c(1980,3),end=c(1981,12))

library(xts)
date <- seq(as.Date("1956/01/01"),by="month",length.out=nrow(don))
don_xts <- xts(don,order.by=date)
don_xts["1990"]
don_xts["1990-8"]

# Extraire des dates
dates_don_xts <- index(don_xts)
dates_don_xts

# use librdate pour les date
an <- year(date)
mois <- months(date) # Commande de base dans R
mois

mois <- month(date) # Commande du package `lubridate`
mois

jour_an <- yday(date)
jour_an

jour_mois <- mday(date)
jour_mois

jour_semaine <- wday(date)
jour_semaine

jour_semaine <- wday(date,label=TRUE)
jour_semaine

########################################################################################################################
                                              VIZ
########################################################################################################################

# avec plot de base
plot(1:nrow(don),don$Elec,
     type="l",col="red",
     xlab="t",ylab="",main="Australian electricity")


# AVce ggplot2
library(ggplot2)

df <- data.frame(x=1:nrow(don),
                 Elec=don$Elec,
                 Elec_M2x12=filter(don$Elec,c(0.5,rep(1,11),0.5)/12))
# 1
ggplot()+
  geom_line(data=df,aes(x=x,y=Elec,colour="1"))+
  geom_line(data=df,aes(x=x,y=Elec_M2x12,colour="2"))+
  xlab("t")+
  ylab(" ")+
  scale_colour_manual(values=c("red","blue"),
                      label=list("Australian electricity","Australian electricity - M(2x)12"),
                      name="Séries temporelles")
# 2
ggplot()+
  geom_line(data=df,aes(x=x,y=Elec,colour="1"))+
  geom_line(data=na.omit(df),aes(x=x,y=Elec_M2x12,colour="2"))+
  xlab("t")+
  ylab(" ")+
  scale_colour_manual(values=c("red","blue"),
                      label=list("Australian electricity","Australian electricity (M12)"),
                      name="Séries temporelles")


# Avec plt
plot.ts(don,xlab="t",ylab="Australian electricity")

# Evidemment, on peut utiliser la commande ts.plot sur des objets ts :
plot.ts(don_ts,xlab="t",ylab="Australian electricity")

# On peut facilement superposer pluseurs séries temporelles au format ts ayant la même fréquence, ici la série et le
# filtrage de cette série par la moyenne mobile \(M_{12}\) :

don_ts_M12 <- filter(don_ts,c(0.5,rep(1,11),0.5)/12)
ts.plot(don_ts,don_ts_M12,xlab="t",col=c(1,2))
legend("topleft",col=c(1,2),lwd=c(1,1),legend=c("Australian electricity","Australian electricity - M(2x)12"))

# On peut utiliser la commande plot.xts sur des objets xts :
plot.xts(don_xts,col=1,main="Australian electricity")

# On peut également superposer pluseurs séries temporelles au format xts, ici la série et le filtrage
# de cette série par la moyenne mobile \(M_{12}\) :
don_xts_M12 <- xts(filter(don_xts,c(0.5,rep(1,11),0.5)/12),order.by=date)
plot.xts(don_xts,col=1,xlab="t")

#enplus 
lines(don_xts_M12,col=2,lwd=3)
legend("topleft",col=c(1,2),lwd=c(1,1),legend=c("Australian electricity","Australian electricity - M(2x)12"))

#############  dygraphs ###############

# Avec dygraphs

# On peut utiliser la commande dygraph du package dygraphs sur des objets ts ou xts :
  
library(dygraphs)
# 1
dygraph(don_ts,xlab="t",ylab="Australian electricity")

# 2
dygraph(don_ts) %>% dyRangeSelector()

# 3
df <- cbind(don_ts,don_ts_M12)
colnames(df) <- c("Elec","Elec M (2x)12")
dygraph(df) %>% dyRangeSelector()



