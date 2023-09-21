quadratic <- function ( a, b, c )
{
  t1 <- sqrt ( b^2 - 4.0 * a * c )
  t2 <- 2.0 * a
  x1 <- - ( b + t1 ) / t2
  x2 <- - ( b - t1 ) / t2

  return ( c ( x1, x2 ) )
}