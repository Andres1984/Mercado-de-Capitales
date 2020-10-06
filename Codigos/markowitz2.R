library(quantmod)
getSymbols(c("AMZN","AAPL","GOOG","MSFT"),src="yahoo",from="2018-07-09",to="2020-10-02")


par(mfrow=c(2,2))
plot(AAPL$AAPL.Close,col="orange", main="AAPL", type="l")
plot(AMZN$AMZN.Close,col = "green", main="AMAZN", type="l")
plot(GOOG$GOOG.Close,col ="blue", main="GOOG", type="l")
plot(MSFT$MSFT.Close, col="red", main="MSFT", type = "l")


# Rendimientos

aapl=Delt(AAPL$AAPL.Close)[-1]
amzn=Delt(AMZN$AMZN.Close)[-1]
goog=Delt(GOOG$GOOG.Close)[-1]
msft=Delt(MSFT$MSFT.Close)[-1]


par(mfrow=c(2,2))
plot(aapl,col="orange", main="AAPL", type="l")
plot(amzn,col = "green", main="AMAZN", type="l")
plot(goog,col ="blue", main="GOOG", type="l")
plot(msft, col="red", main="MSFT", type = "l")

## Media de forma mensual
mamzn=mean(amzn)*20
maapl=mean(aapl)*20
mgoog=mean(goog)*20
mmsft=mean(msft)*20
### Volatilidad de forma mensual
sdamzn=sd(amzn)*sqrt(20)
sdappl=sd(aapl)*sqrt(20)
sdgoog=sd(goog)*sqrt(20)
sdmsft=sd(msft)*sqrt(20)

rend=cbind(amzn,aapl,goog,msft)
colnames(rend)=c("AMZN","AAPL","GOOG","MSFT")


corre=cor(rend)

library(corrplot)
corrplot(corre, method = "number")


mu.vec=cbind(mamzn,maapl,mgoog,mmsft)

sigma2=var(rend)*20


### Portafolio de  mínima varianza

# Crear Matriz A

n=4
one.vec=rep(1,n)
topmat=cbind(2*sigma2,one.vec)
botmat=c(one.vec,0) 
Amat=rbind(topmat,botmat)
bvec=c(rep(0,n),1)
z=solve(Amat)%*%bvec
wmin=z[1:n]
names(wmin)=c("AMZN","AAPL","GOOG","MSFT")
mmin=mu.vec%*%wmin ### Ecuación 19 de las diapositivas
sigma2min=t(wmin)%*%sigma2%*%wmin ### Ecuación 20 de las diapositivas
sigmamin=sqrt(sigma2min)

plot(sigmamin ,mmin, pch=16, cex=2,xlab=expression(sigma[p]), ylab=expression(mu[p]),col="blue",ylim=c(0.01, 0.07), xlim=c(0.07,0.15))
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
points(sdappl,maapl,pch=16, cex=2, col="green" )
points(sdgoog,mgoog,pch=16, cex=2, col="purple" )
text(sdappl ,maapl, labels="AAPL", pos=4)
text(sdgoog ,mgoog, labels="GOOG", pos=4)
text(sigmamin ,mmin, labels="Global min", pos=4)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)

text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)


### Portafolio con la misma rentabilidad de Apple

#Construcción de la Matriz A
top=cbind(2*sigma2,t(mu.vec),one.vec)# Matriz varcovar, el vector de rendimientos y un vector de unos
mit=c(mu.vec,0,0)
bot=c(one.vec,0,0)
Am=rbind(top,mit,bot)
b=c(rep(0,n),mu.vec[2],1)
z=solve(Am)%*%b
wapp=z[1:n]
names(wapp)=c("AMZN","AAPL","GOOG","MSFT")
muapp=mu.vec%*%wapp
sigma2app=t(wapp)%*%sigma2%*%wapp ### Ecuación 20 de las diapositivas
sigmaapp=sqrt(sigma2app)


### Portafolio con la misma rentabilidad de Amazon

#Construcción de la Matriz A
top=cbind(2*sigma2,t(mu.vec),one.vec)# Matriz varcovar, el vector de rendimientos y un vector de unos
mit=c(mu.vec,0,0)
bot=c(one.vec,0,0)
Am=rbind(top,mit,bot)
b=c(rep(0,n),mu.vec[1],1)
z=solve(Am)%*%b
wamzn=z[1:n]
names(wamzn)=c("AMZN","AAPL","GOOG","MSFT")
muamzn=mu.vec%*%wamzn
sigma2amzn=t(wamzn)%*%sigma2%*%wamzn ### Ecuación 20 de las diapositivas
sigmaamzn=sqrt(sigma2amzn)


### Portafolio con la misma rentabilidad de msft

#Construcción de la Matriz A
top=cbind(2*sigma2,t(mu.vec),one.vec)# Matriz varcovar, el vector de rendimientos y un vector de unos
mit=c(mu.vec,0,0)
bot=c(one.vec,0,0)
Am=rbind(top,mit,bot)
b=c(rep(0,n),mu.vec[4],1)
z=solve(Am)%*%b
wmsft=z[1:n]
names(wmsft)=c("AMZN","AAPL","GOOG","MSFT")
mumsft=mu.vec%*%wmsft
sigma2msft=t(wmsft)%*%sigma2%*%wmsft ### Ecuación 20 de las diapositivas
sigmamsft=sqrt(sigma2msft)


### Portafolio con la misma rentabilidad de google

#Construcción de la Matriz A
top=cbind(2*sigma2,t(mu.vec),one.vec)# Matriz varcovar, el vector de rendimientos y un vector de unos
mit=c(mu.vec,0,0)
bot=c(one.vec,0,0)
Am=rbind(top,mit,bot)
b=c(rep(0,n),mu.vec[3],1)
z=solve(Am)%*%b
wgoog=z[1:n]
names(wgoog)=c("AMZN","AAPL","GOOG","MSFT")
mugogg=mu.vec%*%wgoog
sigma2goog=t(wgoog)%*%sigma2%*%wgoog ### Ecuación 20 de las diapositivas
sigmagoog=sqrt(sigma2goog)


##naive

wa=c(0.3,0.4,0.15,0.15)
muna=mu.vec%*%wa
sigma2a=t(wa)%*%sigma2%*%wa ### Ecuación 20 de las diapositivas
sigmaa=sqrt(sigma2a)



### Frontera eficiente
# Covarianza y correlación entre dos retornos de portafolio

sigm.g1=t(wmin)%*%sigma2%*%wapp# Covarianza
rhog1=sigm.g1/(sigmaapp*sigmamin )# Correlación






# Creación de la  frontera eficiente

a=seq(from=-0.5,to=1.5, by=0.05)
n.a=length(a)
z.mat=matrix(0, n.a, n)#Crear la matriz para guardar los resultados delas ponderaciones
colnames(z.mat) = c("AMZN","AAPL","GOOG","MSFT")
mu.z = rep(0, n.a)#Crear el vector para guardar el rendimiento de cada portafolio
sig2.z = rep(0, n.a)#Crear el vector para guardar la varianza de cada portafolio



for (i in 1:n.a) {
  z.mat[i, ] = a[i]*wmin + (1-a[i])*wapp
  mu.z[i] = a[i]*mmin + (1-a[i])*muapp# Ecuación número 9 de la rentabilidad 
  sig2.z[i] = a[i]^2 * sigma2min + (1-a[i])^2 * sigma2app+ 2*a[i]*(1-a[i])*sigm.g1# Ecuación número 10 
}


sig.z=sqrt(sig2.z)










plot(sig.z,mu.z,pch=16,xlab=expression(sigma[p]), ylab=expression(mu[p]),col="brown",ylim=c(0.01, 0.06), xlim=c(0.08,0.12),main="Frontera eficiente")
points(sigmamin ,mmin, pch=16, cex=2,col="blue")
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
points(sdappl,maapl,pch=16, cex=2, col="green" )
points(sdgoog,mgoog,pch=16, cex=2, col="purple" )
points(sigmaa,muna,pch=16,cex=2,col="red")
points(sigmamsft,mumsft ,pch=16, cex=2, col="orange")
points(sigmaamzn,muamzn,pch=16, cex=2, col="yellow" )
points(sigmaapp,muapp,pch=16, cex=2, col="green" )
points(sigmagoog,mugogg,pch=16, cex=2, col="purple" )
text(sdappl ,maapl, labels="AAPL", pos=4)
text(sdgoog ,mgoog, labels="GOOG", pos=4)
text(sigmamin ,mmin, labels="Global min", pos=4)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)
text(sigmaapp,muapp, labels="P1", pos=2)
text(sigmagoog,mugogg, labels="P4", pos=2)
text(sigmaamzn,muamzn, labels="P3", pos=2)
text(sigmamsft,mumsft, labels="P2", pos=2)
text(sigmaa,muna, labels="Naive", pos=4)

library(PerformanceAnalytics)


barplot(z.mat[19,], col="blue")
