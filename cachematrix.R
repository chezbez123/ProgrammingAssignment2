## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The cacheSolve function either computes or returns the cached value of the inverse of the
## special "matrix" returned by makeCacheMatrix.

## This function first initialises x and s, then defines the following objects which can be retured
## from the function as a list:
## set() : sets the value of the special matrix object x
## get() : gets the value of the special matrix object x
## setinverse() : sets the inverse matrix of the special matrix object x
## getinverse() : gets the inverse matrix of the special matrix object x


makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) s <<- solve
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function assigns s to the getinverse object in makeCacheMatrix. If s is not null, the cached value
## is returned. If s is null, the inverse value of x is computed and stored in the setinverse object.

cacheSolve <- function(x, ...) {
      s <- x$getinverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setinverse(s)
      s
}
