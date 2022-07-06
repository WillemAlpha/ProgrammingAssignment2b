## A pair of functions that cache the inverse of a matrix...
## the output of the first function need to be passed to the second
## to run the function use for example: 
  ## x <- matrix(c(4,2,7,6), nrow = 2, ncol =2)
  ## a <- makeCacheMatrix(x)
  ## cacheSolve(a)

## If you type cacheSolve(a) a second time, then the cached inverse matrix will get retrieved without recalculating it



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  get <- function() x ## this function retrieves the original matrix
  set_inv <- function(invertedMatrix) x_inv <<- invertedMatrix  ##this function caches the inverse matrix
  get_inv <- function() x_inv  ##this function retrieves the cache
  list(get = get,
       setInverse = set_inv,
       getInverse = get_inv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## It also gathers the matrix x that was passed to makeCachematrix (even if another "x" is defined in the workspace.)
## This is because of the R lexical scoping rules
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- a$getInverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- a$get()
  x_inv <- solve(data)
  a$setInverse(x_inv)
  x_inv
  
}
