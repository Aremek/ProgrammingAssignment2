#The function below creates a special matrix and defines
#a set of four functions and returns them to the parent 
#environment. These functions are:
#set (it sets a matrix as the argument)
#get  (it retrieves the matrix)
#setsolve (it sets the inverse of the matrix)
#getsolve (it retrieves the inverse of the matrix)
  
makeCacheMatrix <- function (x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#The below function calculates the inverse of the special matrix 
#created by the makeCacheMatrix function. It checks first if the
#inverse matrix has already been calculated. If yes, it skips the
#computation and retrieves data from cache. If no, it calculates
#the inverse matrix and places the outcome in the cache via the
#setsolve function.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
