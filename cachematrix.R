## Here are two functions that create a special object 
## that stores a numeric vector and cache's its mean

##The first function does the following:
## 1. set the value of the matrix (must be invertible)
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##This function solves for the inverse of the matrix created with the first 
##function and sets it in the cache.
##If the inverse is already calculated, it skips the calculation 
##and pulls it from the cache.

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

