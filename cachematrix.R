## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function take a matrix as input and defines some functions
## thant will be apply on that matrix and return a list containing 
## the definition of these functions
## set()   assignes the new matrix to the all the old one and set cached variable to NULL
## get()   returns the matrix in input
## setsolve() assignes a value to the cached variable
## getsolve() returns the value of the cached variable 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
## this function returns the inverse on a matrix passed in input of the above function
## it takes the list retured buy the above function and determine if there is a necessity 
## to re-calcultate the inverse of the matrix in input
## if the value of the cached variable is not null the it assumes that the inverse has been 
## already calculate and return the value of the cached variable
## if the cached variable is null, it assumes that the inverse of this matrix has not been calculate
## the it calculate it and set this new value to the cached variable and return the inverse calculated. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("The inverse of this matrix has already been calculated")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}