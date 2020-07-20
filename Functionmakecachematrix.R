##This function creates a special "matrix" object that can cache its inverse.
##The object store in a variable inv and transfer to in a inverse object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If don´t rescue inverse from makeCacheMatrix function rescue inv and then obtain inverse by solve
cacheSolve <- function(x, ...) {
 
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  obj <- x$get()
  inv <- solve(obj,...)
  x$setInverse(inv)
  inv
}
