par(mfrow=c(1,1))
plot(tempSmooth[1])

library(fda.usc)
int.simpson(tempSmooth[1])

?int.simpson

x<-seq(0,2*pi,length=1001)
fx<-fdata(sin(x)/sqrt(pi),x)
fx0<-fdata(rep(0,length(x)),x)
int.simpson(fx0)
int.simpson(fx)

plot(fx)
plot(fx0)
