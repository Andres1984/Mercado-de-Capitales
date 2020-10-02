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
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4

