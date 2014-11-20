## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix implements a cache structure for a matrix and its inverse
## the structure provides setters and getters for each

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  # if the matrix is modified 
    x <<- y
    m <<- NULL  # the inverse is null and will be calculated if requested
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve implements the inverse of a matrix using the solve method
## the result is stored so that if the inverse is requested twice
## without having modified the matrix the value previously calculated 
## is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() # the previous result is retrieved
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  
  # if no cached value exists
  
  data <- x$get() # retrieve the data to calculate the data
  m <- solve(data, ...)
  x$setsolve(m)  # the result is stored for the next time it is requested
  m
}


# quick test
a<-matrix(runif(25),5,5)
mcm<-makeCacheMatrix(a)
message("no cache")
solveda<-cacheSolve(mcm)
solvedac<-cacheSolve(mcm)
message("loading the same matrix")
mcm<-makeCacheMatrix(a) 
message("no cache")
solveda<-cacheSolve(mcm)
solvedac<-cacheSolve(mcm)
message("loading a newmatrix")
a<-matrix(runif(25),5,5)
mcm<-makeCacheMatrix(a)
message("no cache")
solveda<-cacheSolve(mcm)
solvedac<-cacheSolve(mcm)
