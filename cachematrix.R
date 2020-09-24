## Put comments here that give an overall description of what your
## functions do

####################################
# Usage: makeCacheMatrix(x)
# Arguments: 
#   x   : an object of class matrix (defaults to empty matrix)
# 
# Return value:
#   A list of functions to get and set the x matrix and 
#   to set and get the inverse matrix of the argument x
###################################
makeCacheMatrix <- function(x = matrix()) {
  inv_mem <- NULL
  set <- function(y) {
    x <<- y
    inv_mem <<- NULL
  }
  get <- function() x
  
  setinv <- function(x, ...) {
    inv_mem <<- solve(x, ...)
    inv_mem
  }
  
  getinv <- function() inv_mem
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#################################
# Usage: cacheSolve(x, ...)
# Arguments: 
#   x   : an object created with makeCacheMatrix()
#   ... : further arguments passed to solve()
# Return value:
#   m   : the inverse matrix of the argument x
#################################
cacheSolve <- function(x, ...) {
  stopifnot(is.matrix(x$get()), nrow(x$get()) == ncol(x$get()), !identical(x$get(), matrix()))
  # Throw an error if the matrix of x:
  #   - is not of class matrix or 
  #   - is not a square matrix or 
  #   - is identical to the empty matrix (this is the case when makeCacheMatrix() was called 
  #           without arguments, please use $set to set the matrix or provide the matrix as 
  #           an argument to makeCacheMatrix() before calling cacheSolve())
  
  ## Return a matrix that is the inverse of 'x'
  
  # Read cached inverse
  cached_inverse_mat <- x$getinv()
  
  # If not null then return from cache
  if(!is.null(cached_inverse_mat)) {
    message("Inverse matrix from cache:")
    return(cached_inverse_mat)
  }
  
  # get the matrix to be inverted
  my_mat <- x$get()
  
  # Calculate and cache the inverse matrix of my_mat
  inverse_mat <- x$setinv(my_mat, ...)
  
  # Return the calculated inverse matrix
  inverse_mat
}
