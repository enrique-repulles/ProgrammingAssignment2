#Programming Assignment 2: Lexical Scoping 


## Creates a special "matrix" object that can cache its inverse using lexical scoping.
## The function returns a list with 4 set/get functions pointing to the provided matrix 
## and the cached inverse. The "pointer" is due to the lexical scope.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) cachedInverse <<-inverse
  getinverse <- function () cachedInverse
  
  list (set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## Computes the inverse of the special "matrix" object returned by makeCacheMatrix. 
## The inverse calculation it is not done if the  inverse matrix 
## is cached (previously calculated)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
      message("getting cached inverse matrix")
      return (inverse)
    } 
    # the inverse matrix is not calculated before: 
    matrixData <- x$get()
    inverse <- solve(matrixData)
    x$setinverse(inverse)
    inverse
}
