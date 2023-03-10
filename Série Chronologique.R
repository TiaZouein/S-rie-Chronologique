# ==============================================================================
# =============================== Importations =================================
# ==============================================================================

setwd("~/Documents/TDs/STA202/Projet_2022/JOSEPH_Maxence")
library(xts)




# ==============================================================================
# =================== Traitement et Mise en forme des donn?es ==================
# ==============================================================================

data = read.csv("valeurs_mensuelles.csv", sep=";", header=F, skip = 4)

# Cr?ation de la date
date1 = strptime(c("01/01/1994"), "%m/%d/%Y")
date2 = strptime(c("12/01/2021"), "%m/%d/%Y")
Date = seq(date1, date2, by="1 month")

rev_data = as.data.frame(apply(as.matrix(data$V2), 2, rev))
Naissance = data.frame(Date,rev_data)
names(Naissance) = c("Date", "Taux")

Naissance.xts = xts(Naissance$Taux, order.by=Naissance$Date)
names(Naissance.xts) = c("Taux")



# ==============================================================================
# ======================= Analyse descriptive des donn?es ======================
# ==============================================================================

head(Naissance.xts)
summary(Naissance.xts)
str(Naissance.xts)

plot(Naissance.xts, mar= c(6,4,4,2), major.format="%Y", las=2, xlab ='',ylab = 'Taux de Naissance', main='Taux de naissance mensuel en France entre 1994 et 2021')

# Moyenne
mean(Naissance.xts$Taux)

# Ecart type
sd(Naissance.xts$Taux)

# Boxplot
boxplot(Naissance.xts$Taux, main='bo?te ? moustache de la s?rie de donn?es', col='turquoise')

# Histogramme
hist(Naissance.xts$Taux, breaks = 30, xlab='Taux de Naissance', ylab='Fr?quence', main='Histogramme du Taux de Naissance')
abline(v=12.43, col='red')
legend("topleft", col=c("red"), legend=c("Moyenne"), lty=1)

# Moyenne de la s?rie par mois de l'ann?e
month = format(Naissance$Date, "%m")
mean.month = tapply(Naissance.xts$Taux, as.factor(month), mean)
plot(mean.month, main = "Taux moyen de naissances par mois entre 1994 et 2021", xlab="Mois", ylab = "Taux", pch = 20, type ='b', axes = F)
axis(1,c(1:12),names(mean.month))
axis(2)
grid()

boxplot(Naissance$Taux~month,xlab='Mois', ylab='', col="plum",main='bo?te ? moustache par mois de la s?rie de donn?es', pch=20, cex=0.5)


# Moyenne de la s?rie par an
year = format(Naissance$Date, "%Y")
mean.year = tapply(Naissance.xts$Taux, as.factor(year), mean)
plot(mean.year, main = "Taux moyen de naissances par an entre 1994 et 2021", xlab="Ann?e", ylab = "Taux", pch = 20, type ='b', axes = F)
axis(1,c(1:28),names(mean.year))
axis(2)
grid() # On observe une premi?re tendance ? la baisse, ce qui confirme la premi?re courbe, m?me si l? c'est plus flagrant

boxplot(Naissance$Taux~year,main='Bo?te ? moustache par an de la s?rie de donn?es', xlab='mois', ylab='', col="tan1", pch=20, cex=0.5)



# Autocorr?lation et autocorr?lation partielle

acf(Naissance$Taux, lag.max=50, main="Fonction d'auto-corr?lation") # d?pend ?norm?ment de ce qu'il s'est pass? il y a 12 mois + ph?nom?ne p?riodique
acf(Naissance$Taux, lag.max=50, plot=FALSE) # Il y a en plus une att?nuation : ca d?pend plus de l'ann?e n-1 que de l'ann?e n-2 -> logique
pacf(Naissance$Taux, lag=50, main="Fonction d'auto-corr?lation partielle")

taux = ts(Naissance$Taux)
plot(stats::lag(taux,12), taux, pch=20, cex=0.8, main="Nuage de points de la s?rie retard?e de 12 mois") # variance plus ?lev?e avec lag=7 que lag=12
x=8:14
y=x
lines(x,y) 

plot(stats::lag(taux,7), taux, pch=20, cex=0.8, main="Nuage de points de la s?rie retard?e de 7 mois") # variance plus ?lev?e avec lag=7 que lag=12
x=8:14
y=x
lines(x,y)



# ==============================================================================
# ========================== Estimation de la tendance =========================
# ==============================================================================


# R?gression Lin?aire

time = c(1:nrow(Naissance))

# Lin?aire
reg = lm(Naissance.xts ~ time)
summary(reg)
Taux_chap.lm = reg$fitted # Taux estim? 
plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main ='Tendance estim?e par y=ax+b')
lines(Taux_chap.lm, col='red', lwd=2)
addLegend(legend.loc = "bottomleft", legend.names =c("Taux de Naissance r?el", "Tendance lin?aire estim?e"), col=c("black", "red"), lty=1)

plot(Naissance.xts - Taux_chap.lm, col="orangered2")


# Polynomial de degr? 2
reg2 = lm(Naissance.xts ~ time + I(time^2))
summary(reg2)
Taux_chap2.lm = reg2$fitted
plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main ='Tendance estim?e par y=ax^2+bx+c')
lines(Taux_chap2.lm, col='red', lwd=2)
addLegend(legend.loc = 'bottomleft', legend.names=c('Taux de Naissance r?el', 'Tendance lin?aire estim?e'), col=c('black', 'red'), lty=1)


plot(Naissance.xts - Taux_chap2.lm, col="orangered2")




# Moyenne mobile

h=50
MB = stats::filter(Naissance.xts, filter=array(1/h,dim=h), method = c("convolution"), sides=2, circular=FALSE)
MB.xts = xts(MB, order.by=Naissance$Date)
summary(MB)

plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main='Estimation de la tendance par moyenne mobile')
lines(MB.xts, col='red', lwd=2)
addLegend(legend.loc = 'bottomleft', legend.names=c('Taux de Naissance r?el', 'Moyenne mobile'), col=c('black', 'red'), lty=1)

sd(MB[((h/2)):(length(Naissance$Taux)-(h/2))])

# Noyau Gaussien

noyau = ksmooth(1:length(Naissance$Date), Naissance$Taux, kernel = c("normal"), bandwidth=50)
Taux_chap.ker = xts(noyau$y, order.by=Naissance$Date)
plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main='Estimation de la tendance par noyau gaussien')
lines(Taux_chap.ker, col='red', lwd=2)
#plot(Naissance$Date, Naissance$Taux, xlab='', ylab='Taux de Naissance', main='Estimation de la tendance par noyau gaussien', lty=1)
#lines(Taux_chap.ker, col='red', lwd=2)

plot(Naissance.xts - Taux_chap.ker, col='orangered2')



# Polyn?mes locaux

# degr? 1
lo = loess(Naissance.xts ~ time, degree=1, span= 0.9)
Taux_chap.lo = xts(lo$fitted, order.by=Naissance$Date)

plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main='Estimation de la tendance par des polyn?mes locaux de degr? 1)')
lines(Taux_chap.lo, col='red', lwd=2)

plot(Naissance.xts - Taux_chap.lo, col='orangered2')


# degr? 2
lo = loess(Naissance.xts ~ time, degree=2, span= 0.9)
Taux_chap2.lo = xts(lo$fitted, order.by=Naissance$Date)

plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main='Estimation de la tendance par des polyn?mes locaux de degr? 2)')
lines(Taux_chap2.lo, col='red', lwd=2)

plot(Naissance.xts - Taux_chap2.lo, col='orangered2')



# R?gression sur bases de splines

library(mgcv)
g = gam(Naissance.xts ~ s(time, k=3))
summary(g)

Taux_chap.gam = xts(g$fitted, order.by = Naissance$Date) 
plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main='Estimation de la tendance par r?gression sur bases de splines' )
lines(Taux_chap.gam, col='red')
legend('bottomright', legend=c('Taux de Naissance r?el', 'Tendance estim?e'), col=c('black', 'red'))
# utilise addLegend comme tu l'as fait avant.


# Diff?rentiation

diff.Naissance.xts = diff(Naissance.xts, lag=1, differences = 1) # tendance lin?aire
plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main="Estimation de la tendance par diff?renciation d'ordre 1")
plot(diff.Naissance.xts, col='red')
acf(as.numeric(diff.Naissance.xts), na.action=na.omit, lag.max = 50)


diff2.Naissance.xts = diff(Naissance.xts, lag=1, differences = 2) # tendance polyn?me de degr? 2
plot(Naissance.xts, xlab='', ylab='Taux de Naissance', main="Estimation de la tendance par diff?renciation d'ordre 2")
plot(diff2.Naissance.xts, col='red')
acf(as.numeric(diff2.Naissance.xts), na.action=na.omit, lag.max = 50)




# ==============================================================================
# ======================== Estimation de la saisonnalit? =======================
# ==============================================================================

# On choisi le mod?le de r?gression lin?aire dans le cas quadratique pour enlever la tendance
Naissance.xts.detrend = Naissance.xts - Taux_chap2.lm


# R?gression sur s?rie de Fourier dans le cas quadratique

plot(Naissance.xts.detrend, xlab='', main='Partie saisonni?re et bruit du Taux de naissance')

acf(as.numeric(Naissance.xts.detrend), lag.max=50) # on voit bien la p?riode de 12
pacf(as.numeric(Naissance.xts.detrend), lag.max=50)

w=2*pi/(12*1)
fourier<-cbind(cos(w*time), sin(w*time))
K<-15
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*time), sin(i*w*time))
}

# D?termination de l'ordre q en comparant le coefficient r2 des r?gressions
r2<- NULL
for(i in seq(2, ncol(fourier), by=2))
{
  reg<-lm(Naissance.xts.detrend~fourier[,1:i]-1) #on enl?ve l'intercept, et on fait varier l'ordre i entre 1 et 10
  s <- summary(reg)
  r2  <- c(r2, s$r.squared)
}

plot(r2, type='b', pch=20, main=" Evolution du coefficient R2 en fonction de l'ordre de la d?composition")
grid()
# -> on garde l'ordre 6 d'apr?s le graphe 

reg<-lm(Naissance.xts.detrend~fourier[,1:12]-1)
summary(reg)

Taux_chap2.lm.season = xts(as.numeric(reg$fitted), order.by = Naissance$Date)
plot(Naissance.xts.detrend, main='Estimation de la saisonnalit? avec d?composition en s?rie de fourier')
lines(Taux_chap2.lm.season, col='red', lwd=2)




# On fait pareil dans le cas de la r?gression lin?aire simple

Naissance.xts.detrend0 = Naissance.xts - Taux_chap.lm
plot(Naissance.xts.detrend0, xlab='', main='Partie saisonni?re et bruit du Taux de naissance')

acf(as.numeric(Naissance.xts.detrend0), lag.max=50) # on voit bien la p?riode de 12

w=2*pi/12
fourier<-cbind(cos(w*time), sin(w*time))
K<-15
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*time), sin(i*w*time))
}

# D?termination de l'ordre q en comparant le coefficient r2 des r?gressions
r2<- NULL
for(i in seq(2, ncol(fourier), by=2))
{
  reg<-lm(Naissance.xts.detrend0~fourier[,1:i]-1) #on enl?ve l'intercept, et on fait varier l'ordre i entre 1 et 10
  s <- summary(reg)
  r2  <- c(r2, s$r.squared)
}

plot(r2, type='b', pch=20, main=" Evolution du coefficient R2 en fonction de l'ordre de la d?composition")
grid()
# -> on garde l'ordre 6 d'apr?s le graphe 

reg<-lm(Naissance.xts.detrend~fourier[,1:12]-1)
summary(reg)

Taux_chap.lm.season = xts(as.numeric(reg$fitted), order.by = Naissance$Date)
plot(Naissance.xts.detrend0, main='Estimation de la saisonnalit? avec d?composition en s?rie de fourier')
lines(Taux_chap.lm.season, col='red')

# -> on constate que c'est moins bien




# Moyenne mobile

h = 7
MB.season = stats::filter(Naissance.xts.detrend, filter=array(1/h,dim=h), method=c("convolution"), sides=2, circular=F)
MB.xts.season = xts(MB.season, order.by=Date)

plot(Naissance.xts.detrend, main='Estimation de la saisonnalit? avec la moyenne mobile')
lines(MB.xts.season, col='red', lwd=2)
# j'ai l'impression que c'est grave nul





# Polyn?mes locaux

lo = loess(Naissance.xts.detrend~time, degree=2, span=0.1)
Taux_chap.lo.season = xts(lo$fitted, order.by=Naissance$Date)

plot(Naissance.xts.detrend, main = 'Estimation de la saisonnalit? par polyn?mes locaux')
lines(Taux_chap.lo.season, col='red', lwd=2)


# R?gression sur des bases de splines cycliques
cycle<-c(rep(c(1:12),28))
plot(cycle)

plot(cycle, Naissance.xts.detrend, pch=20)

g<-gam(Naissance.xts.detrend~s(cycle,k=4, bs='cc'))
summary(g)
Taux_chap.gam.season = xts(g$fitted,order.by=Naissance$Date)
plot(Naissance.xts.detrend, main='Estimation de la saisonnalit? par des splines cycliques')
lines(Taux_chap.gam.season,col='red')




# ==============================================================================
# ========================== Comparaison des m?thodes ==========================
# ==============================================================================

# Root-Mean-Squared-Error
rmse = function(u){
  return(sqrt(mean(u^2)))
}

# Mean-Absolute-Error
mae = function(u){
  return(mean(abs(u)))
}


plot(Naissance.xts)
lines(Taux_chap2.lm + Taux_chap2.lm.season, col='purple')
lines(Taux_chap2.lm + Taux_chap.gam.season, col='red')
lines(Taux_chap2.lm + Taux_chap.lo.season, col='turquoise2')
lines(Taux_chap2.lm + MB.xts.season, col='violetred1')

# on remarque que la regr?ssion et la d?composition en base de splines sont meilleurs


# Calcul des erreurs RMSE et MAE pour chaque mod?le

coefficient = matrix(nrow = 3, ncol = 2)
colnames(coefficient) = c("RMSE","MAE")

eps_chap.lm = Naissance.xts - (Taux_chap2.lm + Taux_chap2.lm.season)
plot(eps_chap.lm, col='red')
acf(eps_chap.lm, lag.max=30, main = "ACF des r?sidus de la transformation de Fourier")
acf(eps_chap.lm, main = "ACF des r?sidus de la transformation de Fourier", plot=F)
hist(eps_chap.lm, breaks=20, proba=T)
x = seq(-4,4,length=1000)
lines(x,dnorm(x,0,0.22))
coefficient[1,"RMSE"] = rmse(eps_chap.lm)
coefficient[1,"MAE"] = mae(eps_chap.lm)


eps_chap.gam = Naissance.xts - (Taux_chap2.lm + Taux_chap.gam.season)
plot(eps_chap.gam, col='turquoise2')
acf(eps_chap.gam, main = "ACF des r?sidus de la d?composition en splines")
coefficient[2,"RMSE"] = rmse(eps_chap.gam)
coefficient[2,"MAE"] = mae(eps_chap.gam)


eps_chap.lo = Naissance.xts - (Taux_chap2.lm + Taux_chap.lo.season)
plot(eps_chap.lo, col='orangered')
acf(eps_chap.lo, main = "ACF des r?sidus de la m?thode des polyn?mes locaux")
coefficient[3,"RMSE"] = rmse(eps_chap.lo)
coefficient[3,"MAE"] = mae(eps_chap.lo)

rownames(coefficient) = c('Fourier', 'Spline', 'Polyn?mes locaux')
View(coefficient)


# ==============================================================================
# ============================ Lissage Exponentiel =============================
# ==============================================================================

# Pr?diction 2021

library(forecast)

Naissance0 = as.numeric(Naissance$Taux[1:324])
Naissance1 = as.numeric(Naissance$Taux[325:336]) # Taux de l'ann?e 2021 que l'on cherche ? pr?voir
n0 = length(Naissance0)
n1 = length(Naissance1)

Naissance0.ts = ts(Naissance0, frequency=12)


ets1.ts = ets(y=Naissance0.ts, model="ANN")
ets2.ts = ets(y=Naissance0.ts, model="AAN")
ets3.ts = ets(y=Naissance0.ts, model="AAA")

ets1.ts.forecast = forecast(ets1.ts, h=n1)
ets2.ts.forecast = forecast(ets2.ts, h=n1)
ets3.ts.forecast = forecast(ets3.ts, h=n1)

# pour voir la diff?rence entre ets1 et ets2, on zoom :
plot(1:n1, ets1.forecast$mean, col='red', ylim=c(10.306,10.308))
lines(1:n1, ets2.forecast$mean, col='blue')

plot(Naissance$Date, Naissance$Taux, type='l')
lines(Naissance$Date[(n0+1):(n0+n1)], ets1.ts.forecast$mean, col='red')
lines(Naissance$Date[(n0+1):(n0+n1)], ets2.ts.forecast$mean, col='blue')
lines(Naissance$Date[(n0+1):(n0+n1)], ets3.ts.forecast$mean, col='green')





# Pr?diction 2022

Naissance2022 = as.numeric(Naissance$Taux)
Naissance2022.ts = ts(Naissance2022, frequency=12)

ets1.ts2022 = ets(y=Naissance2022.ts, model="ANN")
ets2.ts2022 = ets(y=Naissance2022.ts, model="AAN")
ets3.ts2022 = ets(y=Naissance2022.ts, model="AAA")


date1_future = strptime(c("01/01/2022"), "%m/%d/%Y")
date2_future = strptime(c("12/01/2022"), "%m/%d/%Y")
Date_future = seq(date1_future, date2_future, by="1 month")
time0 = c(Date, Date_future)

ets1.ts.forecast2022 = forecast(ets1.ts2022, h=length(Date_future))
ets2.ts.forecast2022 = forecast(ets2.ts2022, h=length(Date_future))
ets3.ts.forecast2022 = forecast(ets3.ts2022, h=length(Date_future))

plot(time0[1:length(Date)], Naissance$Taux, type='l')
lines(time0[(length(Date)+1):length(time0)], ets1.ts.forecast2022$mean, col='red')
lines(time0[(length(Date)+1):length(time0)], ets2.ts.forecast2022$mean, col='blue')
lines(time0[(length(Date)+1):length(time0)], ets3.ts.forecast2022$mean, col='green')


# ==============================================================================
# ============================ Holt Winters =============================
# ==============================================================================


hw1 = HoltWinters(Naissance$Taux, gamma=F)
hw1.forecast = predict(hw1, n.ahead=length(Date_future))
plot(time0[1:length(Date)], Naissance$Taux, type='l')
lines(time0[(length(Date)+1):length(time0)], hw1.forecast, col='red')

hw2 = HoltWinters(ts(Naissance$Taux, frequency=12))
hw2.forecast = predict(hw2, n.ahead=length(Date_future))
plot(time0[1:length(Date)], Naissance$Taux, type='l')
lines(time0[(length(Date)+1):length(time0)], hw2.forecast, col='red')












































