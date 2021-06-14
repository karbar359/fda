
# prep
library(fda)
par(mfrow=c(1,1))
set.seed(42)

# 4.1
arg_range = c(0,1)
arg_nbasis = 23
arg_norder = 4
arg_var = 0.25
basis = create.bspline.basis(arg_range, norder = arg_norder, nbasis = arg_nbasis)

fd_coef_const = sin(2*pi*seq(arg_range[1], arg_range[2], length.out = arg_nbasis))
fd_coef_random = rnorm(arg_nbasis, 0, arg_var) # is it correctly defined?
fd_coef = fd_coef_const + fd_coef_random
fd_object = fd(coef = fd_coef, basisobj = basis)

par(mfrow=c(2,2))
for (i in 0:3){
  fd_object_tmp = deriv(fd_object, i)
  plot(fd_object_tmp, main = i)
}

par(mfrow=c(2,2))
for (i in 0:3){
  fd_object_tmp = deriv(fd_object, i)
  plot(
    eval.fd(seq(0,1,length.out = 1001), fd_object_tmp) + rnorm(1001, 0, 0.25), 
    main = i, type="l")
}


# 4.3?
for (i in 0:3){
  fd_object_tmp = deriv(fd_object, i)
  # plot(fd_object_tmp, main = i)
  plot(seq(0,1,length.out = 23), fd_object_tmp$coefs, col = "red", main = i)
  lines(seq(0, 1, length.out = 101), eval.fd(seq(0, 1, length.out = 101), fd_object_tmp))
}


# 4.4, 4.5
N = 20
fd_coefs = matrix(0, nrow = arg_nbasis, ncol = N)
for (i in 1:N){
  fd_coef_const_tmp = sin(2*pi*seq(arg_range[1], arg_range[2], length.out = arg_nbasis))
  fd_coef_random_tmp = rnorm(arg_nbasis, 0, arg_var) # is it correctly defined?
  fd_coef_tmp = fd_coef_const_tmp + fd_coef_random_tmp
  fd_coefs[,i] = fd_coef_tmp
}
fd_object_N = fd(coef = fd_coefs, basisobj = basis)


plot(fd_object_N)
plot(deriv(fd_object_N, 1))
plot(deriv(fd_object_N, 2))


# book
t(fd_object$coefs)

