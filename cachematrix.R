## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its mean.

## This function creates a special "matrix" object that can cache its inverse
## It is a list containing funtions to 
##  1.	set the value of the matrix
##  2.	get the value of the matrix
##  3.	set the inverse of the matrix
##  4.	get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  # 1.	set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #  2.	get the value of the matrix
  get <- function() x
  
  #  3.	set the inverse of the matrix
  setinv <- function(inv) m <<- inv
  
  #  4.	get the inverse of the matrix
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}