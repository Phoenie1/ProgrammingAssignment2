## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
  list(set = set, get = get, setinv=setinv, getinv=getinv)
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)

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
  xinverse
        ## Return a matrix that is the inverse of 'x'
}
