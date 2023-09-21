eulersys <- function ( f, t0, y0, h, n )
{
  t <- t0
  y <- y0
  values <- data.frame ( t = t, t(y0) )

  for ( i in 1 : n )
  {
    y0 <- y0 + h * f ( t0, y0 )
    t0 <- t0 + h
    values <- rbind ( values, data.frame ( t = t0, t(y0) ) )
  }

  return ( values )
}