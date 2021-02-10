## makeCacheMatrix creates object for matrix to be stored
## cacheSolve caches the inverse solve value for later reference

#initialize variables, create get / set functionality
#setmatrix / getmatrix use solve functions to get / set inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
  


  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) i <<- solve
  getmatrix <- function() i
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## caches the solve inverse matrix value for use later by referencing x$getmatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getmatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setmatrix(i)
  i
  
  
}