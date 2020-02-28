## below are 2 functions(makeCacheMatrix and cacheSolve) that cache the inverse of a given matrix
## makeCacheMatrix is a function that creates a matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function is retrieves the inverse of the matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
  }

##Example to test the functions
#matrix <- matrix(c(1:18), nrow = 2, ncol = 2)
#print(matrix)
#CachedMatrix <- makeCacheMatrix(matrix)
#cacheSolve(CachedMatrix)
#Example 2 with diagonal matrix
#diagmatrix <- diag(5,10)
#print(diagmatrix)
#CachedMatrix <- makeCacheMatrix(diagmatrix)
#cacheSolve(CachedMatrix)
