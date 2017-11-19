## Put comments here that give an overall description of what your
## functions do
## The basic idea is to copy the given functions for storing the
## mean of a vector, but substituting for matrix inversion.
## I replaced 'mean' for 'solve' when used as function
## and 'mean' for 'inverse' when used as description.
## I kept using 'm', and not 'i', because of potential imaginary number trouble.

## This function adds a variable 'm' (matrix inverse) to a matrix class
## also it defines functions for getting and getting that matrix inverse
## with the 'set' function, it makes sure that the cache is cleared when
## the matrix is redefined/changed. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the function tries to get the inverse of matrix X
## if it exists, it takes it from the cache created in the above function
## if it does not exist, it gets the matrix data and calculates the inverse
## and then sets it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', preferably from cache
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## example to use: 
## > matrix <- matrix(c(1,2,3,4),2,2)
## > cm <- makeCacheMatrix(matrix)
## > cacheSolve(cm)
## > cacheSolve(cm) # to see that now the data is picked from cache
