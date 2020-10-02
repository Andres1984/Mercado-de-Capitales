library(quantmod)
getSymbols(c("AMZN","MSFT"),src="yahoo",from="2019-01-01",to="2020-09-15")
barChart(MSFT)
barChart(AMZN)

msft=Delt(MSFT$MSFT.Close)[-1]
amzn=Delt(AMZN$AMZN.Close)[-1]


hist(msft,breaks = 80, col="red", main = "MFST")
hist(amzn,breaks = 80, col="blue", main = "AMZN")

plot(msft,col="red",type="l")# Rendimientos
plot(amzn,col="blue",type="l")

# Portafolio mínimo global

mamzn=mean(amzn)*20
mmsft=mean(msft)*20

sdamzn=sd(amzn)*sqrt(20)
sdmsft=sd(msft)*sqrt(20)
corrp=cor(amzn,msft)

## calculo mínimo global pesos amzn

wamzn=(sdmsft^2-corrp*sdamzn*sdmsft)/((sdamzn^2+sdmsft^2)-(2*corrp*sdamzn*sdmsft))
wmsft=1-wamzn

mmin=wamzn*mamzn+wmsft*mmsft
varmin=wamzn^2*sdamzn^2+wmsft^2*sdmsft^2+2*wamzn*wmsft*corrp*sdamzn*sdmsft
sdmin=sqrt(varmin)

### Construcción de la frontera

x.A=seq(from=-0.4, to=1.4, by=0.1)
x.B=1-x.A
mu.p = x.B*mmsft + x.A*mamzn
sig2.p = x.B^2 * sdmsft^2 + x.A^2 * sdamzn^2 + 2*x.A*x.B*corrp*sdmsft*sdamzn
sig.p = sqrt(sig2.p)

## Graficar la frontera eficiente

plot(sig.p,mu.p, pch=16, cex=2,xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 9),"blue", rep("red", 9)),main="Frontera eficiente")
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)
text(x=sdmin, y=mmin, labels="Global min", pos=4)



# Construcción de la CAL y el portafolio óptimo


tbill=0.13/100
tbillmensual=(1+tbill)^(1/12)-1
eramzn=mamzn-tbillmensual
ermsft=mmsft-tbillmensual


wamznop=(eramzn*sdmsft^2-ermsft*corrp*sdamzn*sdmsft)/((eramzn*sdmsft^2+ermsft*sdamzn^2)-((eramzn+ermsft)*corrp*sdmsft*sdamzn))
wmsftop=1-wamznop


#  Indice de sharp

isp=(mu.p-tbillmensual)/sig.p
mu.pop=wamznop*mamzn+wmsftop*mmsft
sig2.pop = wmsftop^2 * sdmsft^2 + wamznop^2 * sdamzn^2 + 2*wmsftop*wamznop*0.74*sdmsft*sdamzn
sig.pop = sqrt(sig2.pop)

isppopt=(mu.pop-tbillmensual)/sig.pop


## Construcción CAL
x.tan = seq(from=0, to=1.5, by=0.1)# wp
mu.p.tan.tbill = tbillmensual + x.tan*(mu.pop- tbillmensual)#CAL
sig.p.tan.tbill = x.tan*sig.pop

## Graficar la frontera eficiente y la CAL

plot(sig.p,mu.p, pch=16, cex=2,xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 9),"blue", rep("red", 9)),main="Frontera eficiente", ylim=c(0, 0.048), xlim=c(0,0.15))
points(sdmsft,mmsft,pch=16, cex=2, col="orange")
points(sdamzn,mamzn,pch=16, cex=2, col="yellow" )
points(sig.p.tan.tbill,mu.p.tan.tbill, pch=16, cex=2, col="pink")
text(x=sig.pop, y=mu.pop, labels="Optimo", pos=4)
text(x=sdmsft, y=mmsft, labels="MSFT", pos=4)
text(x=sdamzn, y=mamzn, labels="AMZN", pos=4)
text(x=sdmin, y=mmin, labels="Global min", pos=4)
text(x=0, y=tbillmensual, labels="Safe", pos=4)
text(x=sig.p.tan.tbill[15], y=mu.p.tan.tbill[15], labels="Risk", pos=4)
