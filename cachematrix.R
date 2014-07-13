## makeCacheMatrix: create list with functions to 
## cache matrix/inverse matrix
## cacheSolve: rerurns inverse matrix from cache 
## or computes (with solve()) and caches inverse matrix
##
## example usage
## > zy<-makeCacheMatrix()
## > zx<-as.matrix(c(1, 2, 3,4))
## > dim(zx)<-c(2,2)
## > zx
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > zy$set(zx)
## > zy$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(zy)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(zy)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix is a function that creates a list
## of functions that are used with caching the inverse
## of a matrix to reduce the computational load for 
## matrices that will used multiple times.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ## assign functions to set, get, setinverse, and getinverse.
  ## These will be used as elements of a list for use with matrices. 
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  ## create list with functions described above
  ## for use with matrices and their cached
  ## inverse matrices

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The cacheSolve function is used to check if the matrix "x"
## has already had its inverse matrix computed. If not compute
## and cache the inverse matrix of "x".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  ## m is the local copy of the inverse matrix that may have 
  ## been previously cached for matrix "x"
  m <- x$getinverse()

  ## If the inverse matrix has been previously computed, return the
  ## cached inverse matrix. This should reduced the computational
  ## load if the inverse of matrix "x" is used multiple times. 

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## get the matrix and set "data" to the matrix.
  data <- x$get()
  
  ## If the inverse of matrix "x" has not been computed, use the solve()
  ## function to compute the inverse matrix a  
  ## note: solve(a, b, ...), If missing, b is taken to be an identity matrix 
  ## and solve will return the inverse of a.
  m <- solve(data)

  ## cache m for further use. m is the local value of the inverse matrix 
  ## m will be cached by invoking the setinverse function.
  x$setinverse(m)
  m
}
