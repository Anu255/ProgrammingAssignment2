
-##This function function makeCacheMatrix gets a matrix as an input, sets the value of the matrix,
  -#gets the value of the matrix, sets the inverse Matrix and gets the inverse Matrix. The matrix object
  -#can cache its own object. 
  -
  -#<<- operator is used to assign a value to an object in an environment that is different 
  -#from the current environment 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinvmean = getinvmean)
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
-# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
  -# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
  -# and set the invertible  matrix by using the solve function.
  -# In case inverse matrix from makeCacheMatrix, it returns a message  "Getting Cached Invertible Matrix" 
  -#and the cached object
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }
