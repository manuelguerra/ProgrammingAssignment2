## Functions detremine inverse of a matrix and saves to cache. 
## If inverse already exists, it is retrieved from cache.
## Otherwise, it calculates inverse.

## This function sets the inverse function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<-  NULL
  }
  get <- function() x
  setinverse <- function(solve) m <-- solve
  getinverse <- function() m
  list(set=set, get=get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse or retrieves the inverse from Cache

cacheSolve <- function(x, ...) {
  m <- x$getinv
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
}
