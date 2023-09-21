newton <- function ( f, fp, x, tol = 1.0e-3, m = 100 )
{
  iter <- 0
  oldx <- x + 10.0 * tol

  while ( tol < abs ( x - oldx ) )
  {
    iter <- iter + 1
    if ( m < iter )
    {
      warning ( 'newton: iteration limit exceeded.' )
      break
    }

    oldx <- x
    x <- x - f ( x ) / fp ( x )
  }