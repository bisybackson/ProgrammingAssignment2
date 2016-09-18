## These are short functions that calculate the inverse of a square
## invertible matrix, and caches the result in variable i. It makes
## use of lexical scoping to access the correct variables at run time
## and pushes  responses to the parent environment with <<- operator.

## makeCacheMatrix instantiates the variables i to NULL, x to the matrix
## as passed in, and contains the various getters and setters. It returns
## a list with all the instantiated functions named, making them accessible
## with the $ operator.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve gets passed the matrix, then calls the $getinverse function.
## If i is not NULL, it means the inverse was precalcuated and is returned
## from the makeCacheMatrix's value of i; otherwise it calculates the inverse
## and sets it using the $setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...);
  x$setinverse(i)
  i
}