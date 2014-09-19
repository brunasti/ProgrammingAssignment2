## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). 
## This module implements a pair of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("-- getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)  
}



## This function enable to execute a quick test of the functions and to explain how to use them
test <- function() {

  
  message("Generate a small random Invertible Matrix")
  ##  set.seed(123)
  I=2
  n=3
  time=matrix(NA,ncol=3,nrow=I)
  for(i in 1:I){
    t0<-proc.time()
    testX<-solve(matrix(runif(n^2),n))
    mt1<-proc.time()
    time[i,]<-(mt1-t0)[1:3]
  }
  
  message("Invertible Matrix")
  message(testX)
  
  tmx <- makeCacheMatrix( x = testX )
  tmx$x <- testX
  
  message("Cache Invertible Matrix")
  message( tmx$x )
  
  message("first get of the inverted Matrix")
  message( cacheSolve(tmx) )
  
  message("second get of the inverted Matrix")
  message( cacheSolve(tmx) )
}
