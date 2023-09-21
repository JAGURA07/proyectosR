jacobi <- function ( A, b, tol = 10e-7, maxiter = 100 )
{
  n <- length ( b )
  iter <- 0

  Dinv <- diag ( 1.0 / diag ( A ) )
  R <- A - diag ( diag ( A ) )
  x <- rep ( 0.0, n )
  newx <- rep ( tol, n )

  while ( tol < vecnorm ( newx - x ) )
  {
    if ( maxiter < iter )
    {
      warning ( "jacobi: Iteration maximum exceeded." )
      break
    }
    x <- newx
    newx <- Dinv %*% ( b - R %*% x )
    iter <- iter + 1
  }

  return ( as.vector ( newx ) )
}