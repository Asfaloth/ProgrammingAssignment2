## The makeCacheMatrix function creates a special matrix out of a usual matrix, 
## which is able to store another matrix in a cache. 
## The cacheSolve function computes the inverse of a makeCacheMatrix variable,
## but first checks if the desired matrix is already stored in the cache to
## reduce computation time.

## makeCacheMatrix defines a list of four functions to set resp. get the matrix
## resp. its cache (its inverse).

makeCacheMatrix <- function(x = matrix()) {
      ## initialize the cache for the inverse of x with NULL since it has not 
      ## yet been computed
      s <- NULL
      ## a function to set the value of the matrix. The cache is set to
      ## NULL since the inverse of the new matrix has not yet been computed
      set <- function(y) {
            x <<-y
            s <<- NULL
      }
      ## a funtion to get the value of the matrix itself
      get <- function() x
      ## a funtion to set the value of the cache
      setsolve <- function(solve) s <<- solve
      ## a funtion to get the value of the cache
      getsolve <- function() s
      ## return a list of all four defined functions
      list(set = set, 
           get = get, 
           setsolve = setsolve, 
           getsolve= getsolve)
}


## cacheSolve gives the inverse of a matrix stored as a makeCacheMatrix
## by first checking if the inverse already is stored in the cache.
## If this is not the case it computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
      ## read out the cache of x
      s <- x$getsolve()
      ## if the cache contains a matrix, then this matrix is returned and 
      ## the function terminates
      if (!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      ## otherwise the matrix is stored in a variable
      data <- x$get()
      ## the inverse of the matrix is computed
      s <- solve(data, ...)
      ## the inverse is stored in the cache
      x$setsolve(s)
      ## the inverse is returned
      s
}
