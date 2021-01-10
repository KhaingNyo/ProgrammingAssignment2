##The first function(makeCacheMatrix) creates a special "matrix" object that can cache its inverse
#1.Set the value of the matrix
#2.Get the value of the matrix 
#3.Set the value of the inverse
#4.Get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
    #Initialize objects
    i <- NULL
    #Assign the input argument to the x object
    #Assign the value of NULL to the s object
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    #Define the get for the matrix x
    get <- function() x
    
    #Define the setter for the inverse s
    setinverse <- function(inverse) i <<- inverse
    
    #Define the get for the inverse s
    getinverse <- function() i
    
    #Assign each of these functions
    #as an element within a list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

##If the inverse has already been calculated,then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #If s is NULL, get the matrix from the input object
  data <- x$get()
  
  #Calculate the inverse
  i <- solve(data, ...)
  
  #Set the inverse in the input object
  x$setinverse(i)
  
  #Return the value of the inverse
  i
}

##testing for functions
B <- matrix(c(2,4,6,8),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

B <- matrix(c(3,2,0,0,0,1,2,-2,1),3,3)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
