## Put comments here that give an overall description of what your
## functions do

## makeCacheeMatrix is a function that creates a list
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
  m <- x$getinverse()

  ## If the inverse matrix has been previously computed, return the
  ## cached inverse matrix. This should reduced the computational
  ## load if the inverse of matrix "x" is used multiple times. 

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## If the inverse of matrix "x" has not been computed, use the solve()
  ## function to compute the inverse matrix and cache it for further 
  ## use. 

  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
