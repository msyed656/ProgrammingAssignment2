## Put comments here that give an overall description of what your
## functions do
# This function makes a special matrix that can remember its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This is where we will keep the inverse (if we find it)
  
  # This lets us set a new matrix
  set <- function(y) {
    x <<- y      # update the matrix
    inv <<- NULL # forget the old inverse because the matrix changed
  }
  
  # This just gives back the matrix we set
  get <- function() {
    x
  }
  
  # This saves the inverse after we calculate it
  setInverse <- function(i) {
    inv <<- i
  }
  
  # This gives back the inverse if we already saved it
  getInverse <- function() {
    inv
  }
  
  # We return all these functions in a list so we can use them later
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# This function checks if the inverse is already calculated
# If not, it calculates it and saves it for next time
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # check if the inverse is already there
  
  # If the inverse is already stored, just use that one
  if (!is.null(inv)) {
    message("getting cached data")  # tell us it's using the stored one
    return(inv)
  }
  
  # If we don't have it yet, get the matrix and solve it
  data <- x$get()
  inv <- solve(data, ...)  # this finds the inverse
  
  # Save the new inverse so we can use it later without solving again
  x$setInverse(inv)
  
  # Return the inverse we just found
  inv
}}
