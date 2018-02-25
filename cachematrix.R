## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## function to set a new input matrix value
  ## and reset inv to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## function to return the input matrix
  get <- function() x
  
  ## function to set the value of inv to the
  ## inverted matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## function to return inv (either NULL or the
  ## inverted matrix)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)


}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         ## get the value of inv from makeCacheMatrix()
  inv <- x$getInverse()
  
        ## If inv is not NULL, it is already caching the inverted
		## matrix for the same input matrix. Return inv and
		## stop further execution of this function.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## The remaining code will only execute if inv is NULL.
		
  ## Get the input matrix from makeCacheMatrix().
  mat <- x$get()
  
  ## Calculate the inverse of the matrix.
  inv <- solve(mat, ...)
  
  ## Pass the inverted matix to makeCacheMatrix() so
  ## that inv in makeCacheMatrix() gets updated.
  x$setInverse(inv)
  
  ## Return the calculated inversion.
  inv			
}
