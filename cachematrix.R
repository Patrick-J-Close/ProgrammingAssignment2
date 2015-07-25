##Create a matrix that can cache inverse of a matrix 
##Return a list of functions to:
##1. Set value of the matrix
##2. Get the value of the matrix
##3. Set the value of the inverse of the matrix
##4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x =matrix()){
     
     ##Initialize matrix 
     inv <- NULL
     
     ##Assign a value to an object in a different environment
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     setinv <- function(matrixInv) inv <<- matrixInv
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
     
}

##Using list of functions defined in makeCacheMatrix:
##Check if inverse of matrix defined in makeCacheMatrix is 
##cached and, if not, compute it

cacheSolve <- function(x, ...) {
     
     inv <- x$getinv()
     
     ##Determine if inverse has already been calcluated and cached
     ##If true, retrieve inverse from cache
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     ##Get matrix and compute its inverse
     data <- x$get()
     inv <- solve(data, ...)
     
     ##Set value of inverse in the cache
     x$setinv(inv)
     
     ##Print inverse
     return(inv)
}
