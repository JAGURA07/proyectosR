fun <- function(x) return(4 * x^4)
curve(fun, 0, 1)
abline(h = 0, lty = 2)
abline(v = c(0, 1), lty = 2)

quadrature <- function(fun, a, b, tol=1e-8) {
    # numerical integration using adaptive quadrature

  simpson2 <- function(fun, a, b) {
    # numerical integral using Simpson's rule
    # assume a < b and n = 2
    return((b-a)/6 * (fun(a) + 4*fun((a+b)/2) + fun(b)))
  }
 
    quadrature_internal <- function(S.old, fun, a, m, b, tol, level) {
        level.max <- 100
        if (level > level.max) {
            cat ("recursion limit reached: singularity likely\n")
            return (NULL)
        }
        S.left <- simpson2(fun, a, m) 
        S.right <- simpson2(fun, m, b)
        S.new <- S.left + S.right
        if (abs(S.new-S.old) > tol) {
            S.left <- quadrature_internal(S.left, fun, 
                                          a, (a+m)/2, m, tol/2, level+1)
            S.right <- quadrature_internal(S.right, fun, 
                                           m, (m+b)/2, b, tol/2, level+1)
            S.new <- S.left + S.right
        }
        return(S.new)
    }
 
    level = 1
    S.old <- (b-a) * (fun(a) + fun(b))/2
    S.new <- quadrature_internal(S.old, fun, 
                                 a, (a+b)/2, b, tol, level+1)
    return(S.new)
}

quadrature(fun, 0, 1)