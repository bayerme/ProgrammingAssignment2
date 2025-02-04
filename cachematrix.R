## Put comments here that give an overall description of what your
## functions do
# creating 2 functions, makeCacheMatrix and cacheSolve:
# makeCacheMatrix is has 2 'get-ers' and 2 'set-ers' for the input matrix and its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
  #clearing out cached value
  cached_value <- NULL
  
  #defining 'set' function using super-assignment operator
  set <- function(set_matrix){
    matrix <<- set_matrix
    cached_value <<- NULL
  }
  
  #defining 'get' function
  get <- function() matrix
  
  #defining 'setInverse' function
  setInverse <- function(inverse) cached_value <<- inverse
  
  #defining 'getInverse' function
  getInverse <- function() cached_value
  
  #creating a list to assign functions
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
  }


## Write a short comment describing this function
# cacheSolve takes an matrix and retrives the cached inverse or calculates and caches the inverse
cacheSolve <- function(input_matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- input_matrix$getInverse()
  
  # returning cached value if it exists
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # if cached value does not exist, store the inverse of the input in the cache
  data <- input_matrix$get()
  inverse <- solve(data)
  input_matrix$setInverse(inverse)
  inverse
}
