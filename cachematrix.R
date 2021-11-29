##  Assignment:  Caching the Inverse of the Matrix
##  makeCachematrix is a function that makes a special matrix object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse)  inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

##  cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

# Generate a matrix that is the invrse of matrix x
inv <- x$getinverse()
if(!is.null(inv)) {
      message("getting cached result")
      return(inv)
  }
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv
}
