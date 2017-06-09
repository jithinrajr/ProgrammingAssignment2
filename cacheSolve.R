cacheSolve <- function( x = matrix())
{
  i <- x$getinverse()
  if (!is.null(i)){
          message("getting cached matrix")
          return(i)
  }
  
  data <- x$get()
  i    <- solve(data)
  x$setinverse(i)
  i
  
 
}