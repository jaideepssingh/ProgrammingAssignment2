## Taking benefits of caching for costly computation like Matrix inversion by 
## For this purpose I have written two functions that cache the inverse of a matrix.


## Function 1: makeCacheMatrix() 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## x is a square invertible matrix
## return- a list containing functions: set matrix, get matrix, set the inverse matrix, 
## and get the inverse matrix. This list serves as the input to cacheSolve().
  
  inv <- NULL
  set <- function(y) {
    x <<- y    
    inv <<- NULL
## '<<-' is used to assign value to an object in an enviornment different from current.

  }
  get <- function() x 
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function 2: cacheSolve()
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## x- output of makeCacheMatrix.
## return- inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinv()  

##if the inverse has been calculated
  if(!is.null(inv)) {
    message("getting cached data")  ##gets inverse from cache and skips the computation.
    return(inv)
  }

## if inverse not calculated, computation is done below.
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  x$setinv(inv)  ## sets the value of the inverse in the cache
  
  return(inv)   ## Return a matrix that is the inverse of 'x'
}