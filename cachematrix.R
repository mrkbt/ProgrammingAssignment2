# The following pair of funcions provides means to 
# cache an inverse of a matrix. The first function
# creates an object that is a list of four functions.
# To create the cacheable matrix use:
#
#   mat <- makeCacheMatrix(your_matrix)
#
# To change the matrix you can reassign it:
#
#   mat <- makeCacheMatrix(new_matrix)
#
# or you can use the set function:
#
#   mat$set(new_matrix)
#
# To get the matrix or its inverse use:
#
# mat$get() and mat$getinverse() respectively.
#
# The inverse is calculated by calling
#
#   cacheSolve(mat).
#
# It will cache and return the inverse. If it is
# called again it will return the cached inverse, 
# unless the matrix was change with one of the 
# abovementioned methods.

makeCacheMatrix <- function(x = matrix()) {
# Creates a matrix for which its inverse can be 
# cached and provides the functions to retrieve 
# the matrix and its inverse and the functions to
# change the matrix and to change the cached inverse.
  inverse <- NULL
  set <- function (y = matrix()) {
    x <<- y
    inverse <<- NULL
  }
  get <- function () x
  setinverse <- function (inv) inverse <<- inv
  getinverse <- function () inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

cacheSolve <- function(x, ...) {
  # Returns a matrix that is the inverse of 'x'.
  # If it is cached, it is retrieved, if not, 
  # it is calculated.
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  matrix_data <- x$get()
  inverse <- solve(matrix_data, ...)
  x$setinverse(inverse)
  inverse
}
