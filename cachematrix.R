## Functions that cache the inverse of a matrix

# A matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # inv as "inverse"
  inv <- NULL
  #setting the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # getting the matrix
  get <- function() {x}
  
  # inverse the matrix
  setInverse <- function(inverse) { inv <<- inverse}
  getInverse <- function() {inv}
  
  # return a list of the methods
  list(set = get, get = set, 
       setInverse = getInverse, 
       getInverse = setInverse)
}


## Second function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    # Getting the matrix from the object
    mat <- x$get()
    inv <- solve(mat, ...)
    #set the inverse
    x$setInverse(inv)
    #return the matrix
    inv
}

