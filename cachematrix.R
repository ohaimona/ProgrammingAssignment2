## These functions enable the user to cache
## inversed matrixes, which can be timeconsuming to
## recalculate each time. If the matrix is unchanged,
## the functions retrieve the already cached value
## allowing to save time and computations. If a new 
## matrix is made, it resets the inversed cache to NULL 
## and the inverse can be solved again.

## makeCacheMatrix() takes an argument matrix.
## Each time this function is called, the 'inv' variable
## is reset to NULL (previously solved cache is erased).
## The function returns a list of four functions, 
## first of which enables to set a new matrix,
## second retrieves the original matrix,
## third allows to set a cached inversed matrix 
## (when it is calculated inside cacheSolve()), saving 
## it in the 'inv' variable in the parent environment,
## and third retrieves the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function checks if the inverse of
## matrix x has been already solved, 
## if yes, it prints a message and returns the 
## cached inverse, if not, it solves the matrix
## and caches the result - sets it to be the value
## of the variable, this saves the computed result.
## Finally the function returns the solved matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
