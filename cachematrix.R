makeCacheMatrix <- function(x = matrix()) {
  elc <- NULL
  set <- function(y) {
    x <<- y
    elc <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) elc <<-solve
  getinverse <- function() elc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getinverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setinverse(invFunc)
  invFunc
}
## Return a matrix that is the inverse of 'x'
##test###
z<- matrix(1:4,2,2)
a<- makeCacheMatrix(z)
cacheSolve(a)
