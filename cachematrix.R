## makeCacheMatrix and cacheSolve functions are intended to be used together to cache matrix inversion operations
## See individual function descriptions for usage details.

## makeCacheMatrix "augments" a matrix passed as an argument, 
## by returning a list of 4 functions that should be used to interact with underlying matrix
## Usage examples: myMatrix <- matrix(1:10, 1:5)
## Augment the matrix: myAugmentedMatrix <- makeCacheMatrix(myMatrix)
## Get underlying data: myAugmentedMatrix$get() (result is myMatrix)
## Overwrite underlying data: myAugmentedMatrix$set(newMatrix)
## Get computed inverse: myAugmentedMatrix$getInverse() (not to be used directly by user, use cacheSolve instead)
## Set an inverse matrix: myAugmentedMatrix$setInverse(inverse) (not to be used directly by user)

makeCacheMatrix <- function(x = matrix()) {
  #variable for storing inverse
  inverse <- NULL
  #function to override underlying matrix data
  set <- function(y) {
    x <<- y
    #if overriding data, delete the cached inverse result
    inverse <<- NULL
  }
  #function to get the underlying matrix data
  get <- function() x
  #function to set the inverse
  setInverse <- function(inv) inverse <<- inv
  #function to get the inverse
  getInverse <- function() inverse
  #return all functions as a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve() function computes an inverse of a matrix, caches the result and returns in
## cacheSolve() accepts "augmented" matrix as an argument (a matrix created with makeCacheMatrix() function)

cacheSolve <- function(x, ...) {
  # let's try to get previously calculated inverse matrix
  inverse <- x$getInverse()
  # check if anything was returned
  if(!is.null(inverse)) {
    #if returned, just pass it over again
    return(inverse)
  }
  #if not, get underlying data and solve the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  #cache the inverse and return
  x$setInverse(inverse)
  inverse
}
