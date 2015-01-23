## Two functions are supplied to reduce the time to calculate the inverse of a matrix.
## Starting from a regular matrix, the function makeCacheMatrix will generate a
## 'special' matrix, which contains the original matrix and a cached value m of the
## inverse matrix. Both the original matrix and the cached value can be retrieved and set
## by the given get and set functions.
## The function cacheSolve takes this 'special' matrix as argument, and returns the 
## inverse of the original matrix. If this inverse was already calculated, and hence
## stored in the cache, it will simply return this cached memory. Otherwise, it will 
## explicitly calculate the inverse, store it in the cache, and return this inverse.
##
## ------ Example ------
## To calculate the inverse of a given matrix mat, the following steps need to be undertaken:
## mat_cache <- makeCacheMatrix(x)
## cacheSolve(mat_cache)

## Function makeCacheMatrix
## -- Arguments --
##    x: a regular matrix object, default initialized is the empty matrix
## -- Returns --
##    a 'special' matrix (a list), which contains the cached inverse of the matrix
##    in the variable m, and four more functions to access the matrix and its inverse:
##    i) the set function resets the original matrix
##    ii) the get function returns the original matrix
##    iii) the setSolve function resets the inverse matrix m
##    iv) the getSolve function returns the inverse matrix m

makeCacheMatrix <- function(x = matrix()) {
  # Variable containing whether an inverse matrix is cached
  m <- NULL
  
  # set function: sets a new original matrix
  set <- function(y){
    x <<- y # store the new matrix...
    m <<- NULL # ...and reset the cached memory
  }
  
  # get function: returns the original matrix
  get <- function() x
  
  # setSolve function: stores the inverse matrix in m
  setSolve <- function(inverse) m <<- inverse
  
  # getSolve function: returns the (cached) inverse matrix m
  getSolve <- function() m
  
  # make the special matrix (list)
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}


## Function cacheSolve
## -- Arguments --
##    x: the special matrix object constructed by makeCacheMatrix
##    ...: possible extra arguments, fed to the solve() function
## -- Returns --
##    m, the inverse of the matrix x. Only if the matrix has been changed since
##    the last call, m is calculated. Otherwise, the cached value is returned to
##    save computational effort

cacheSolve <- function(x, ...) {
  # Return the cached value for the inverse of x via the getSolve function
  m <- x$getSolve()
  # Verify whether this cached value is not NULL, i.e. that the inverse is already calculated
  if(!is.null(m)) {
    # If the inverse m is already calculated, notify the user, and return this inverse
    # without calculating it again
    message("getting cached data")
    return(m)
  }
  # !! This part is only accessed if no cached inverse was found !!
  # First store the original matrix (accessed via the get function) in data
  data <- x$get()
  # Then calculate the inverse of this matrix
  m <- solve(data, ...)
  # Cache the inverse of this matrix using the setSolve function
  x$setSolve(m)
  # Return this inverse
  m
}