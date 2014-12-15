##  Caching the Inverse of a Matrix
## Matrix inversion caching rather than computing it repeatedly 
## Contains a pair of functions that cache/recall the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL        #to store the inverse matrix
  set <- function(y) {   #assigns the original matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x   #returns the matrix
  setinverse <- function(solvedinverse) #assigns the inverse matrix
    inverse <<- solvedinverse
  getinverse <- function() inverse  #returns the inverse matrix
  list (set =  set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve either computes, returns and stores the inverse of a matrix or retrieves the previously computed result. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)){  #use cached version if available
    message("getting cached inverse")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)  #calculate inverse matrix
  x$setinverse(inverse)   #store inverse in cache
  inverse
}
