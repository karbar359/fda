
# prep
library(fda)
par(mfrow=c(1,1))
set.seed(42)

# 5.1 a, b
# arg_range = c(0, 1)
arg_range = c(-5, 5)
arg_lenth = 101
arg_var = 0.25

arg_seq = seq(arg_range[1], arg_range[2], length.out = arg_lenth)
# arg_data_const = sin(2*pi*arg_seq)
arg_data_const = exp((-1)*arg_seq^2 /2)
arg_data_random = rnorm(arg_lenth, 0, arg_var)
arg_data = arg_data_const + arg_data_random

# c
arg_nbasis = 23
arg_norder = 4
basis = create.bspline.basis(arg_range, norder = arg_norder, nbasis = arg_nbasis)
fd_par_deriv_pen = 1
lambda = 10^(1)
fd_par = fdPar(basis, fd_par_deriv_pen, lambda)

# d
fd_object = smooth.basis(argvals = arg_seq, y = arg_data, fdParobj = fd_par)
plot(fd_object, ylim = c(min(arg_data), max(arg_data)))
points(arg_seq, arg_data, col = "red")

# e
MSE = sqrt(
  sum((arg_data - eval.fd(arg_seq, fd_object$fd))^2) 
    / (arg_lenth - fd_object$df))

c(MSE, arg_var)

# f
test_lambda <- function(lambda){
  fd_par_deriv_pen = 1
  fd_par = fdPar(basis, fd_par_deriv_pen, lambda)

  fd_object = smooth.basis(argvals = arg_seq, y = arg_data, fdParobj = fd_par)
  plot(fd_object, ylim = c(min(arg_data), max(arg_data)))
  points(arg_seq, arg_data, col = "red")
  
  MSE = sqrt(
    sum((arg_data - eval.fd(arg_seq, fd_object$fd))^2) 
    / (arg_lenth - fd_object$df))
  
  GCV = fd_object$gcv
  c(MSE, GCV)
}

i_range <- -4:5
cnt <- 1
MSEs <- rep(0, length(i_range))
GCVs <- rep(0, length(i_range))
for (i in i_range){
  MSEs[cnt] <- test_lambda(10^i)[1]
  GCVs[cnt] <- test_lambda(10^i)[2]
  cnt <- cnt + 1
}

i_range[which.min((abs(MSEs-arg_var)))]

# g
plot(i_range, GCVs)


# 5.2
