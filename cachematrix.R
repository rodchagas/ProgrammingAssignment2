#Assignment: Caching the Inverse of a Matrix
## Put comments here that give an overall description of what your
## functions do
#Assignment: Caching the Inverse of a Matrix
#Description
#Matrix inversion is usually a costly computation and there may be some benefit to caching 
#the inverse of a matrix rather than compute it repeatedly (there are also alternatives to
#matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x #get function
      setInverse <- function(solveMatrix) inv <<- solveMatrix#calculate the inverse
      getInverse <- function() inv #get inverse
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
      
}