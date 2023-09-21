fun <- function(x0,y0) return(x=1+(x0-y0)^2)

midpoint<- function ( fun, x0, y0=1, h, n )
{
  x <- x0
  y <- y0

  for ( i in 1 : n )
  {
    s1 <- h*fun(x0,y0)
    s2 <- h*fun( x+h/2, y0+s1/2 )
    y0 <- y0+s2
    x0 <- x0+h
    x <- c(x,x0)
    y <- c(y,y)
  }

  return (data.frame(x))
}

midpoint(fun,1,1,0.1,10)