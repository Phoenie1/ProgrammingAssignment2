## Just like the example, makeCacheMatrix is structured identically,
## with set and get to set and get, respectively, getinv and setinv 
## to get and set the inverse, but setinv should not be used by itself,
## it is called by the cacheSolve function.  Both makeCacheMatrix and 
## cacheSolve should only be fed an invertable matrix.  No error checking
## is done, per the examples.
##

## makeCacheMatrix - generates a list with a matrix and its inverse,
## Inverse set to all NA until the cacheSolve function is called
## against it.

makeCacheMatrix <- function(x = matrix()) {
  d <- dim(x)
  xinv <- matrix(, nrow=d[1], ncol=d[2])
  print(x)
  print(xinv)
  set <- function(y) {
    x <<- y
    d <- dim(y)
    xinv <<- matrix(, nrow=d[1], ncol=d[2])
  }
  get <- function() x
  getinv <- function() xinv
  setinv <- function(inverse) xinv <<- inverse
  list(set = set, get = get, setinv=setinv, getinv=getinv)
}

## Function to calculate and cache the inverse of a matrix

cacheSolve <- function(x, ...) {
  xinverse <- x$getinv()
  if( !anyNA(xinverse) ) {
    print("Retrieving From Cache")
    return(xinverse)
  }
  library('MASS')
  g <- x$get()
  xinverse <- ginv(g)
  x$setinv(xinverse)
        ## Return a matrix that is the inverse of 'x'
  xinverse
}
