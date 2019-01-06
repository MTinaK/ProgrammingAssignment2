## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache the inverse of its matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  M2 <- NULL ##Set as null so can be used within function later
  
  #setter/getter for function
  
  set <- function(y){
    x <<- y
    M2 <- Null
  }
  get <- function() x
  
  #setter/getter for inverse matrix
  setinv <- function(solveMatrix) M2 <<- solveMatrix
  getinv <- function() M2
  
  # creates a list with the functions as arguments
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse of the matrix (M2) that has been created within 
## the function makeCacheMatrix and returns the result to makeCacheMatrix. 
## If the inverse has already been computed before, it gets the cached 
## data that are stored in makeCacheMatrix if called again.

cacheSolve <- function(x, ...) {
  
  #return "getting cached data and inverse matrix if inverse matrix is not Null
  M2 <- x$getinv()
  if(!is.null(M2)) {
    message("getting cached data")
    return(M2)
  }
  
  #If matrix is not inversed, get the matrix and compute its inverse
  data <- x$get()
  M2 <- solve(data, ...) 
  
  # Call setinv from makeCacheMatrix
  x$setinv(M2)
  
  # return result
  M2
}
