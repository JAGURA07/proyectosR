fun <- function(x) return(sqrt(x))
curve(fun, 0, 1)
abline(h = 0, lty = 2)
abline(v = c(0, 1), lty = 2)

trapezoid.vec <- function(f.vec, h = 0.01) {
  n <- length(f.vec) 
  return(h*(f.vec[1]/2 + sum(f.vec[2:(n-1)]) + f.vec[n]/2))
}

trapecio<- function(fun, a = 0, b = 1, n = 100) {
  h <- (b-a)/n
  x.vec <- seq(a, b, by = h)
  f.vec <- sapply(x.vec, fun)
  return(trapezoid.vec(f.vec, h))
}

trapecio(fun, 0, 1, 20)