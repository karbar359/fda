


library(fda)

nbasis = 17
norder = 6
months = cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))
fbasis = create.fourier.basis(c(0,365),21)
bbasis = create.bspline.basis(c(0,365),nbasis,norder,months)


data(daily)

argvals = (1:365)-0.5
fdParobj = fdPar(fbasis,int2Lfd(2),1e-2)
tempSmooth = smooth.basis(argvals,daily$tempav,fdParobj)$fd
par(mfrow=c(1,1))
plot(tempSmooth)

temppca=pca.fd(tempSmooth$fd,nharm=4,fdParobj)
par(mfrow=c(2,2))
plot(temppca)

# models
linmod()
fRegress()

?fRegress
class(daily$precav)
annualprec <- log10(apply(daily$precav, 2,sum))
test <- fRegress(annualprec ~ tempSmooth)

test$betaestlist
test$yfdobj
test$yhatfdobj
test$yfdobj - test$yhatfdobj
test$wt
test$df
test$bvar
test$gcv
test$betastderrlist

test$Cmatinv
test$y2cMap
test$SigmaE
fRegress.stderr()


test.mdl1 <- fRegress(annualprec ~ tempSmooth, method="model")

nbetabasis2 <- 11
betabasis2  <- create.fourier.basis(c(0, 365), nbetabasis2)
betafd2     <- fd(rep(0, nbetabasis2), betabasis2)
betafdPar2  <- fdPar(betafd2, lambda=10)

test.mdl2 <- test.mdl1
test.mdl2[['betalist']][['tempSmooth']] <- betafdPar2

test2 <- do.call('fRegress', test.mdl2)

test[['df']]
test2[['df']]

RMSE1 <- sqrt(mean(with(test, (yhatfdobj-yfdobj)^2)))
RMSE2 <- sqrt(mean(with(test2, (yhatfdobj-yfdobj)^2)))


annualprec.fit2 <- test2$yhatfdobj
plot(annualprec.fit2, annualprec, type="p", pch="o")
lines(annualprec.fit2, annualprec.fit2, lty=2)

plot(test2$betaestlist[[2]])



xfdlist <- list(const=rep(1, 35), tempSmooth=tempSmooth)

# The intercept must be constant for a scalar response
betabasis1 <- create.constant.basis(c(0, 365))
betafd1    <- fd(0, betabasis1)
betafdPar1 <- fdPar(betafd1)

betafd2     <- create.bspline.basis(c(0, 365),7)
# convert to an fdPar object
betafdPar2  <- fdPar(betafd2)

betalist <- list(const=betafdPar1, tempSmooth=betafdPar2)

test3   <- fRegress(annualprec, xfdlist, betalist)
annualprec.fit3 <- test3$yhatfdobj
#  plot the data and the fit
plot(annualprec.fit3, annualprec, type="p", pch="o")
lines(annualprec.fit3, annualprec.fit3)
plot(test3$betaestlist[[2]])



daybasis65 <- create.fourier.basis(
  rangeval=c(0, 365), nbasis=65,
  axes=list('axesIntervals'))
Temp.fd <- with(
  CanadianWeather,
  smooth.basisPar(
    day.5, daily$tempav, daybasis65)$fd)
TempRgn.f <- fRegress(Temp.fd ~ region, CanadianWeather)

TempRgn.mdl <- fRegress(Temp.fd ~ region, CanadianWeather, method='m')

TempRgn.m <- do.call('fRegress', TempRgn.mdl)



region.contrasts <- model.matrix(~factor(CanadianWeather$region))
rgnContr3 <- region.contrasts
dim(rgnContr3) <- c(1, 35, 4)
dimnames(rgnContr3) <- list(
  '', 
  CanadianWeather$place, 
  c('const', paste('region', c('Atlantic', 'Continental', 'Pacific'), sep='.')))

const365 <- create.constant.basis(c(0, 365))
region.fd.Atlantic <- fd(matrix(rgnContr3[,,2], 1), const365)
# str(region.fd.Atlantic)
region.fd.Continental <- fd(matrix(rgnContr3[,,3], 1), const365)
region.fd.Pacific <- fd(matrix(rgnContr3[,,4], 1), const365)
region.fdlist <- list(
  const=rep(1, 35),
  region.Atlantic=region.fd.Atlantic,
  region.Continental=region.fd.Continental,
  region.Pacific=region.fd.Pacific)
# str(TempRgn.mdl$betalist)

beta1 <- with(Temp.fd, fd(basisobj=basis, fdnames=fdnames))
beta0 <- fdPar(beta1)
betalist <- list(
  const=beta0, 
  region.Atlantic=beta0,
  region.Continental=beta0, 
  region.Pacific=beta0)

TempRgn <- fRegress(Temp.fd, region.fdlist, betalist)

plot(TempRgn$yfdobj)
plot(TempRgn$yhatfdobj)
