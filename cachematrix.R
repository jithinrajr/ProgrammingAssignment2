
## Create a matrix object to cache the inverse and a function to 
## calculate it or return the cached object

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## This function creates a special "matrix" object that can cache its 
## inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
       i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
