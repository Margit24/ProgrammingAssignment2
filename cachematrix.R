## Matrix inversion is usually a costly computation and there may be benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## The following two functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to:
  ## set the value of the matrix to NULL
  ## get the value of the matrix
  ## set the value of the inverted matrix
  ## get the value of the inverted matrix

makeCacheMatrix <- function (x = matrix()) {
  m <- NULL                                   
  setmatrix <- function(y){                          ## set value of matrix
      x <<- y                                        ## allow cacheSolve to check for changes  
      m <<- NULL                                     ## set value of m to NULL
      }
getmatrix <-function() x                             ## get value of matrix
setinverse <- function(solve) m <<- solve            ## set value of inverted matrix
getinverse <- function () m                          ## get value of inverted matrix
list(setmatrix = setmatrix,                          ## creates list for functions
     getmatrix = getmatrix, 
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix 
  ## It first checks if the inverse has already been computed
  ## If so (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
  ## If not, it computes the inverse, sets the value in the cache via setinverse function

cacheSolve <- function (x=matrix(), ...){             ## compare matrix
  m<- x$getinverse()                                  ## get inversed matrix if present
  if(!is.null(m)) {                                   ## checks if cacheSolve has run before
        message("getting cached matrix")
        return (m)
  }                                                   ## else
  matrix <- x$getmatrix()                             ## get values of input matrix
  m <- solve(matrix, ...)                             ## compute inverse of input matrix
  x$setinverse(m)                                     ## run setinverse function
  m                                                   ## return inversed matrix 
}