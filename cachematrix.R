## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this function has 4 functions to set and get the matrix and inverser
# once the inverse is calcualated, the value is saved in cache and returned in the future
makeCacheMatrix <- function(x = matrix()) {
  # setting the initial value to null
  cached <- NULL
  
  setMatrix <- function(newMatrix){
    # clearing the cache incase a previous value existed  
    cached <<- NULL
    x <<- newMatrix
  }
  
  # return the matrix
  getMatrix <- function() { x }
  
  # return the cashed value
  getInverse <- function() { cached }
  
  setInverse <- function(solve) 
  {
    cached <<- solve
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## this function calcaulte the inverser of a matrix created by the cuntion makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## get the cached value
  matrixInverse <- x$getInverse()
  
  # if the value is not null, return the cached value 
  if (!is.null(matrixInverse))
  {
    message("getting the cashed matrixInverse:")
    return (matrixInverse)
  }
  
  # else, get the matrix and calculate the inverse and call setInvrese to save the value in cache
  data <- x$getMatrix()
  matrixInverse <- solve(data)
  x$setInverse(matrixInverse)
  
  # return the inverse
  matrixInverse
}
