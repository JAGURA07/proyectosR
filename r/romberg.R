fun <- function(x) return(sqrt(x))
curve(fun, 0, 1)
abline(h = 0, lty = 2)
abline(v = c(0, 1), lty = 2)

trapezoid.vec <- function(f.vec, h = 0.01) {
  n <- length(f.vec) 
  return(h*(f.vec[1]/2 + sum(f.vec[2:(n-1)]) + f.vec[n]/2))
}

trap<- function(fun,a,b,n) {
  h <- (b-a)/n
  x.vec <- seq(a, b, by = h)
  f.vec <- sapply(x.vec, fun)
  return(trapezoid.vec(f.vec, h))
}

romberg <- function(f,a,b,m) {
  R <- matrix(NA, nrow = m, ncol = m)
  R[1, 1] <- trap(f,a,b,m)
  for (j in 2:m) {
    R[j,1] <- trap(f,a,b,m)
    for (k in 2:j) {
      k4 <- 4^(k-1)
      R[j,k] <- k4 * R[j,k-1] - R[j-1,k-1]
      R[j,k] <- R[j,k] / (k4-1)
    }
  }
  return (R[m,m])
}

romberg(fun,0,4,20)