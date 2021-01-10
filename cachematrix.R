##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

##  If the inverse has already been calculated,then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##testing for functions
B <- matrix(c(2,4,6,8),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

B <- matrix(c(3,2,0,0,0,1,2,-2,1),3,3)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
