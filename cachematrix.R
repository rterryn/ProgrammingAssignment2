## A set of functions for Programming Assignment 2, R-Programming Course, 
## Coursera Data Science track, 12-27-16. The overall task is to compose
## 2 functions which calculate and cache the inverse of a matrix. An initial
## call to solve will calculate and cache the matrix
## the calculation will retrieve the cached matrix instead of re-calculate 
## the inverse.   


## Function "makeCaheMatrix" creates a special "matrix" object that can cache
## its inverse. makeCacheMatrix contains 4 functions: set, get, setinverse,
## and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  nVerse <- NULL ## Create a 'null' variable to represent the matrix inverse.
  set <- function(y){ ##changes the vector stored in the parent function.
    x <<- y 
    nVerse <<- NULL 
  }
  get <- function() x ## returns vector stored in the parent function.
  setInverse <- function(solve) nVerse <<- solve ## stores the value of the input as variable "nVerse"
  getInverse <- function() nVerse## returns the value stored by 'setInverse'
  list(set = set, get = get, ## return "special 'matrix'" as a list of functions.
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function "cacheSolve" checks to see if a matrix inverse has already been calculated
## for the input matrix. If yes then it retrives it, if not it calculates and stores
## the matrix inverse for the current input. This function assumes all input matrices
## are invertable, and "makeCacheMatrix" has been called on the input matrix prior to
## calling "cacheSolve"

cacheSolve <- function(x, ...) {
  nVerse <- x$getInverse()
  if(!is.null(nVerse)){
    message("getting cached data")
    return(nVerse) ## Return a previously stored matrix that is the inverse of 'x'
  }
  data <- x$get()
  nVerse <- solve(data)
  x$setInverse(nVerse)
  nVerse ## Return a newly calculated matrix that is the inverse of 'x'
}
