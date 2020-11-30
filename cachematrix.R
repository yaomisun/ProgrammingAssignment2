## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  local_ix <- NULL
  set <- function(y) {
    x <<- y
    local_ix <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed_x) local_ix <<- inversed_x
  getinverse <- function() local_ix
  list(set = set,          # gives the name 'set' to the set() function defined above
       get = get,          # gives the name 'get' to the get() function defined above
       setinverse = setinverse,  # gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse)  # gives the name 'getinverse' to the getinverse() function defined above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  check_ix <- x$getinverse()
  if(!is.null(check_ix)) {
    message("getting cached data")
    return(check_ix)
  }
  data <- x$get()
  check_ix <- solve(data)
  x$setinverse(check_ix)
  check_ix
}
