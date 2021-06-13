
# prep
library(fda)
par(mfrow=c(1,1))
set.seed(42)

arg_range = c(-5, 5)
arg_nbasis = 23
arg_norder = 4
basis = create.bspline.basis(arg_range, norder = arg_norder, nbasis = arg_nbasis)
fd_par = fdPar(basis, 3, 10^(-0.5))

arq_seq = seq(arg_range[1], arg_range[2], length.out = arg_nbasis)
arg_data = exp((-1)*arq_seq^2 /2)
plot(arq_seq, arg_data)

smooth.basis(y = arg_data, fdParobj = fd_par)

fd_object = fd(coef = fd_coef, basisobj = basis)
plot(fd_object)
