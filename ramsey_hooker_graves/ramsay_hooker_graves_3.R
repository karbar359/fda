
# prep
library(fda)

# 3.1
par(mfrow=c(1,1))
basis = create.bspline.basis(c(0, 1), norder = 4, nbasis = 23)
fd_object = fd(
  coef = sin(2*pi*seq(0, 1, length.out = 23)), 
  basisobj = basis)
class(fd_object)
plot(basis)
plot(fd_object)

par(mfrow=c(2,2))
for (i in 0:3){
  plot(deriv(fd_object, i), main = i)
}


# book
# create.fourier.basis(rangeval=c(0, 1), nbasis=3,
#   period=diff(rangeval), dropind=NULL, quadvals=NULL,
#   values=NULL, basisvalues=NULL, names=NULL,
#   axes=NULL)
# create.bspline.basis(rangeval=NULL, nbasis=NULL,
#   norder=4, breaks=NULL, dropind=NULL, quadvals=NULL,
#   values=NULL, basisvalues=NULL,
#   names="bspl", axes=NULL)
# create.constant.basis(c(0,1))
# create.monomial.basis(c(0,1), 4) 
# create.exponential.basis
# create.polygonal.basis
# create.power.basis

methods(class="basisfd")
# basisfd(type, rangeval, nbasis, params,
#   dropind=vector("list", 0),
#   quadvals=vector("list", 0),
#   values=vector("list", 0),
#   basisvalues=vector("list", 0))

# $nbasis 
# $dropind 
# $quadvals 
# $basisvalues 
# $values 
