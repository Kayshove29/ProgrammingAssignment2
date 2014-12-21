## What the makeCacheMatrix function does is that it creates 
## a special matrix object that can cache its inverse
## The way the function works isn't all that different from the makeVector and
## cachemean function
makeCacheMatrix <- function(x = matrix()) {
  InverseX <- NULL
  set <- function(y) {
    x <<- y
    InverseX <<- NULL
  }
  get <- function() x
  Set_inverse<- function(inverse) InverseX <<-inverse
  Get_inverse <- function() InverseX
  list(set = set, get = get,
       Set_inverse = Set_inverse,
       Get_inverse = Get_inverse)
}

## The function cacheSolve returns the inverse of a matrix A created by
## the makeCacheMatrix function.
## But iff the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseX <- x$getinverse()
  if (!is.null(InverseX)) {
    message("retrieving cached inverse matrix")
    return(InverseX)
  } else {
    InverseX <- solve(x$get())
    x$setinverse(InverseX)
    return(InverseX)
  }
}
