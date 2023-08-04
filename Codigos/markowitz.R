library(quantmod)
getSymbols(c("AMZN","AAPL","GOOG","MSFT"),src="yahoo",from="2018-07-09",to="2020-07-09")


library(quantmod)
library(dygraphs)
library(tidyverse)
library(lubridate)
library(htmlwidgets)
getSymbols("SPY", from="2016-01-01", to="2020-01-01")


SPY <- SPY[,c(1:4)] ## remove the volume and adjusted columns
SPY$SMA50 <- SMA(Cl(SPY), n = 50) #create SMA50 line
p <- dygraph(SPY, xlab = "Date", ylab = "Price", main = "SPY Price") %>%
  dySeries("SPY.Open", label = "Open", color = "black") %>%
  dySeries("SPY.Low", label = "Low", color = "red") %>%
  dySeries("SPY.High", label = "High", color = "green") %>%
  dySeries("SPY.Close", label = "Close", color = "orange") %>%
  dySeries("SMA50", label = "SMA50", color = "blue") %>%
  dyRangeSelector() %>%
  dyCandlestick()%>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = T)  %>%
  dyRoller(rollPeriod = 1)
p

getSymbols(c("CRUD.L", "CVX","XOM"),src="yahoo",from="2018-07-09",to="2020-07-09")


wti=Delt(CRUD.L$CRUD.L.Close)[-1]
cvx=Delt(CVX$CVX.Close)[-1]
xom=Delt(XOM$XOM.Close)[-1]

# Rendimientos

aapl=Delt(AAPL$AAPL.Close)[-1]
amzn=Delt(AMZN$AMZN.Close)[-1]
goog=Delt(GOOG$GOOG.Close)[-1]
msft=Delt(MSFT$MSFT.Close)[-1]


par(mfrow=c(2,2))
plot(wti,col="red")
plot(cvx,col="blue")
plot(xom,col="green")



par(mfrow=c(2,2))
plot(aapl,col="orange", main="AAPL", type="l")
plot(amzn,col = "green", main="AMAZN", type="l")
plot(goog,col ="blue", main="GOOG", type="l")
plot(msft, col="red", main="MSFT", type = "l")




maapl=mean(aapl)*20
mamzn=mean(amzn)*20
mgoog=mean(goog)*20
mmsft=mean(msft)*20

sdaapl=sd(aapl)*sqrt(20)
sdamzn=sd(amzn)*sqrt(20)
sdgoog=sd(goog)*sqrt(20)
sdmsft=sd(msft)*sqrt(20)

rend=cbind(amzn,aapl,goog,msft)
colnames(rend)=c("AMZN","AAPL","GOOG","MSFT")


library(corrplot)
corp=cor(rend)
corrplot(corp, method = "pie")
mu.vec=cbind(mamzn,maapl,mgoog,mmsft)
colnames(mu.vec)=c("AMZN","AAPL","GOOG","MSFT")
sigma2=var(rend)*20# Matriz de Varcovar



# Portafolio mínimo global

## Crear matriz A
n=4
one.vec=rep(1,n)
topmat=cbind(2*sigma2,one.vec)
botmat=c(one.vec,0)
Amat=rbind(topmat,botmat)
bvec=c(rep(0,n),1)
z=solve(Amat)%*%bvec
wmin=z[1:n]
names(wmin)=c("AMZN","AAPL","GOOG","MSFT")
mmin=mu.vec%*%wmin
sigma2min=t(wmin)%*%sigma2%*%wmin
sigmamin=sqrt(sigma2min)



### Rentabilidad deseada 6%
one.vec=rep(1,n)
topmat=cbind(2*sigma2,t(mu.vec),one.vec)
mitmat=c(mu.vec,0,0)
botmat=c(one.vec,0,0)
Amat=rbind(topmat,mitmat,botmat)
#En b yo defino mi rentabilidad
bvec=c(rep(0,n),0.06,1)
z=solve(Amat)%*%bvec
w6=z[1:n]
names(w6)=c("AMZN","AAPL","GOOG","MSFT")
mu6=mu.vec%*%w6
sigma26=t(w6)%*%sigma2%*%w6
sigma6=sqrt(sigma26)



### Rentabilidad AMZN

one.vec=rep(1,n)
topmat=cbind(2*sigma2,t(mu.vec),one.vec)
mitmat=c(mu.vec,0,0)
botmat=c(one.vec,0,0)
Amat=rbind(topmat,mitmat,botmat)
#En b yo defino mi rentabilidad
bvec=c(rep(0,n),mu.vec[1],1)
z=solve(Amat)%*%bvec
wAMZN=z[1:n]
names(wAMZN)=c("AMZN","AAPL","GOOG","MSFT")
muAMZN=mu.vec%*%wAMZN
sigma2AMZN=t(wAMZN)%*%sigma2%*%wAMZN
sigmaAMZN=sqrt(sigma2AMZN)


### Rentabilidad AAPL

one.vec=rep(1,n)
topmat=cbind(2*sigma2,t(mu.vec),one.vec)
mitmat=c(mu.vec,0,0)
botmat=c(one.vec,0,0)
Amat=rbind(topmat,mitmat,botmat)
#En b yo defino mi rentabilidad
bvec=c(rep(0,n),mu.vec[2],1)
z=solve(Amat)%*%bvec
wAAPL=z[1:n]
names(wAAPL)=c("AMZN","AAPL","GOOG","MSFT")
muAAPL=mu.vec%*%wAAPL
sigma2AAPL=t(wAAPL)%*%sigma2%*%wAAPL
sigmaAAPL=sqrt(sigma2AAPL)


### Rentabilidad MSFT

one.vec=rep(1,n)
topmat=cbind(2*sigma2,t(mu.vec),one.vec)
mitmat=c(mu.vec,0,0)
botmat=c(one.vec,0,0)
Amat=rbind(topmat,mitmat,botmat)
#En b yo defino mi rentabilidad
bvec=c(rep(0,n),mu.vec[4],1)
z=solve(Amat)%*%bvec
wMSFT=z[1:n]
names(wMSFT)=c("AMZN","AAPL","GOOG","MSFT")
muMSFT=mu.vec%*%wMSFT
sigma2MSFT=t(wMSFT)%*%sigma2%*%wMSFT
sigmaMSFT=sqrt(sigma2MSFT)


### Rentabilidad GOOG

one.vec=rep(1,n)
topmat=cbind(2*sigma2,t(mu.vec),one.vec)
mitmat=c(mu.vec,0,0)
botmat=c(one.vec,0,0)
Amat=rbind(topmat,mitmat,botmat)
#En b yo defino mi rentabilidad
bvec=c(rep(0,n),mu.vec[3],1)
z=solve(Amat)%*%bvec
wGOOG=z[1:n]
names(wGOOG)=c("AMZN","AAPL","GOOG","MSFT")
muGOOG=mu.vec%*%wGOOG
sigma2GOOG=t(wGOOG)%*%sigma2%*%wGOOG
sigmaGOOG=sqrt(sigma2GOOG)


# Reparticion igual


w=c(0.3,0.1,0.1,0.6)

mi=mu.vec%*%w
si=t(w)%*%sigma2%*%w
si=sqrt(si)


plot(sigmamin ,mmin, pch=16, cex=2,xlab=expression(sigma[p]), ylab=expression(mu[p]),col="blue",ylim=c(0.01, 0.07), xlim=c(0.07,0.15))
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
points(sdaapl,maapl,pch=16, cex=2, col="green" )
points(sdgoog,mgoog,pch=16, cex=2, col="purple" )
points(sigmaAMZN ,muAMZN ,pch=16, cex=2, col="yellow" )
points(sigmaAAPL,muAAPL,pch=16, cex=2, col="green" )
points(sigmaMSFT,muMSFT,pch=16, cex=2, col="orange" )
points(sigma6,mu6 ,pch=16, cex=2, col="brown" )
points(sigmaGOOG,muGOOG,pch=16, cex=2, col="purple" )
text(sigmaAAPL,muAAPL, labels="PAAPL", pos=2)
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)
text(x=sigmaMSFT, y=muMSFT, labels="PMSFT", pos=3)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)
text(sigmamin ,mmin, labels="Global min", pos=4)
text(sigmaAMZN ,muAMZN, labels="PAMZN", pos=2)
text(sdgoog ,mgoog, labels="GOOG", pos=4)
text(sigmaGOOG ,muGOOG, labels="PGOOG", pos=2)
text(sdaapl ,maapl, labels="AAPL", pos=4)
text(sigma6,mu6, labels="P6", pos=4)



## Curva de Frontera eficiente



sigm.g6=t(wmin)%*%sigma2%*%w6 #Covarianza entre el portafolio mínimo global y el portafolio con
# rentabilidad igual a 6%
rhog6=sigm.g6/(sigma6*sigmamin)# Correlación



a=seq(from=-1.5,to=1.5, by=0.05)
n.a=length(a)
z.mat=matrix(0, n.a, n)
colnames(z.mat) = c("AMZN","AAPL","GOOG","MSFT")
mu.z=rep(0, n.a)
sig2.z=rep(0, n.a)


for (i in 1:n.a) {
  z.mat[i, ] = a[i]*wmin + (1-a[i])*w6#Generar los pesos de la matriz Z
  mu.z[i] = a[i]*mmin + (1-a[i])*mu6# Ecuación número 9 de la rentabilidad de cada portafolio
  sig2.z[i] = a[i]^2 * sigma2min + (1-a[i])^2 * sigma26 + 2*a[i]*(1-a[i])*sigm.g6# Ecuación número 10 para la varianza
}


sig.z=sqrt(sig2.z)# Volatilidad

library(PerformanceAnalytics)

chart.StackedBar(z.mat, xaxis.labels=round(sig.z,digits=3), xlab="Portafolio", ylab="Pesos",main="Ponderaciones")


plot(sigmamin ,mmin, pch=16, cex=2,xlab=expression(sigma[p]), ylab=expression(mu[p]),col="blue",ylim=c(0.01, 0.07), xlim=c(0.07,0.15),main="Frontera eficiente")
points(sig.z,mu.z,pch=16,cex=1.5, col="cyan")
points(si,mi,pch=16,cex=2,col="violet")
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
points(sdaapl,maapl,pch=16, cex=2, col="green" )
points(sdgoog,mgoog,pch=16, cex=2, col="purple" )
points(sigmaAMZN ,muAMZN ,pch=16, cex=2, col="yellow" )
points(sigmaAAPL,muAAPL,pch=16, cex=2, col="green" )
points(sigmaMSFT,muMSFT,pch=16, cex=2, col="orange" )
points(sigma6,mu6 ,pch=16, cex=2, col="brown" )
points(sigmaGOOG,muGOOG,pch=16, cex=2, col="purple" )
text(sigmaAAPL,muAAPL, labels="PAAPL", pos=2)
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)
text(x=sigmaMSFT, y=muMSFT, labels="PMSFT", pos=3)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)
text(sigmamin ,mmin, labels="Global min", pos=4)
text(sigmaAMZN ,muAMZN, labels="PAMZN", pos=2)
text(sdgoog ,mgoog, labels="GOOG", pos=4)
text(sigmaGOOG ,muGOOG, labels="PGOOG", pos=2)
text(sdaapl ,maapl, labels="AAPL", pos=4)
text(sigma6,mu6, labels="P6", pos=4)


# Portafolio óptimo

TBILL=0.11/100
Tbillmensual=(1+TBILL)^(1/12)-1


mu.minus.rf = mu.vec-Tbillmensual*one.vec
topvec=solve(sigma2)%*%t(mu.minus.rf)
botvec=t(one.vec)%*%topvec
t=topvec[,1]/botvec
muop=mu.vec%*%t
sigma2op=t(t)%*%sigma2%*%t
sigmaop=sqrt(sigma2op)

sr.t = (muop - Tbillmensual)/sigmaop
sr.t


## Crear la CML capital market line

# Portafolio usando la ecuación número 1 
x.t = seq(0, 2, by=0.1)
mu.pe = Tbillmensual + x.t*(muop - Tbillmensual )# Ecuación número 4 
sig.pe = x.t*sigmaop 




plot(sigmamin ,mmin, pch=16, cex=2,xlab=expression(sigma[p]), ylab=expression(mu[p]),col="blue",ylim=c(0, 0.1), xlim=c(0,0.25),main="Frontera eficiente")
points(sigmaop,muop,pch=16, cex=2, col="brown1")
points(sig.pe,mu.pe,pch=16,cex=1.5, col="deeppink")
points(sig.z,mu.z,pch=16,cex=1.5, col="cyan")
points(si,mi,pch=16,cex=2,col="violet")
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
points(sdaapl,maapl,pch=16, cex=2, col="green" )
points(sdgoog,mgoog,pch=16, cex=2, col="purple" )
points(sigmaAMZN ,muAMZN ,pch=16, cex=2, col="yellow" )
points(sigmaAAPL,muAAPL,pch=16, cex=2, col="green" )
points(sigmaMSFT,muMSFT,pch=16, cex=2, col="orange" )
points(sigma6,mu6 ,pch=16, cex=2, col="brown" )
points(sigmaGOOG,muGOOG,pch=16, cex=2, col="purple" )
text(sigmaAAPL,muAAPL, labels="PAAPL", pos=2)
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)
text(x=sigmaMSFT, y=muMSFT, labels="PMSFT", pos=3)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)
text(sigmamin ,mmin, labels="Global min", pos=4)
text(sigmaAMZN ,muAMZN, labels="PAMZN", pos=2)
text(sdgoog ,mgoog, labels="GOOG", pos=4)
text(sigmaGOOG ,muGOOG, labels="PGOOG", pos=2)
text(sdaapl ,maapl, labels="AAPL", pos=4)
text(sigma6,mu6, labels="P6", pos=4)
text(sigmaop,muop,labels = "Optimo", pos=2)
text(0,Tbillmensual,labels = "safe", pos=4)
text(sig.pe[16],mu.pe[16],labels = "Risk",pos=2)

