## Put comments here that give an overall description of what your
## functions do
# The function makeCacheMatrix() takes as imput a matrix x and 
# returns a list of operations on its internally stored objects x 
# (a matrix) and inv_x (a cached copy of the inverse of matrix x).
# The operations are getters and setters for the internal objects.
# Only setinv() calculates the inverse of x and stores it in inv_x.
#
# The function cacheSolve() takes as input an object created by
# makeCacheMatrix() and returns the inverse of its internal matrix x.
# At first it tries to get the inverse of x from cache and
# if it fails it then computes the inverse and stores it to the cache
# for next time.
#


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
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  
  setinv <- function(x, ...) {
    inv_x <<- solve(x, ...)
    inv_x
  }
  
  getinv <- function() inv_x
  
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


## Example #############################################################
# Set the seed for reproducability
# set.seed(123)
#
# Set n. Do not set too large because inversion will take for ever
# n <- 3000
#
# Create a square matrix A with random uniform numbers
# A <- matrix(runif(n^2),n)
#
# Create a cache matrix object by calling makeCacheMatrix()
# cmat <- makeCacheMatrix(A)
#
# Calculate the inverse of A with cacheSolve() and cache the result
# system.time({
#   z1 <- cacheSolve(cmat)
# })
#
# Take the inverse from cache
# system.time({
#   z2 <- cacheSolve(cmat)
# })
#
# Just checking that the two results are the same
# identical(z1,z2)
##########################################################################