fun <- function(x) return(sqrt(x))
curve(fun, 0, 1)
abline(h = 0, lty = 2)
abline(v = c(0, 1), lty = 2)

simp38 <- function(f, a, b, m = 100) {
    x.ends = seq(a, b, length.out = m + 1)
    y.ends = f(x.ends)
    x.midh = (2 * x.ends[2:(m + 1)] + x.ends[1:m]) / 3
    x.midl = (x.ends[2:(m + 1)] + 2 * x.ends[1:m]) / 3
    y.midh = f(x.midh)
    y.midl = f(x.midl)

    p.area = sum(y.ends[2:(m+1)] + 3 * y.midh[1:m] + 3 *
                     y.midl[1:m] + y.ends[1:m])
    p.area = p.area * abs(b - a) / (8 * m)
    return(p.area)
}

simp38(fun,1,2)