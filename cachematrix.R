## To cache the inverse of an inversible square matrix so that when we need it 
## again, it can be looked up in the cache rather than recomputed. It is able 
## to cache potentially time-consuming computations.

## The first function, makeCacheMatrix creates a special "matrix" object that 
## can cache the inverse of a square inversible matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function, cacheSolve calculates the inverse of a square 
## inversible matrix created with the above function. However, it first checks 
## to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the mean 
## of the data and sets the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
