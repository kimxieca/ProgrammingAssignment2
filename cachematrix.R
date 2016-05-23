
   ## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
  
  ## Write a short comment describing this function
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of the inverse
  ## 4. get the value of the inverse
  
  makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
  }



## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}