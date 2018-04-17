## makeCacheMatrix & cacheSolve - functions to calculate, store and retrieve the inverse of a given matrix

## makeCacheMatrix - This function is used to store and retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  
  set.matrix <- function(y)
  {
    x <<- y
    inverse.matrix <<- NULL
  }
  
  get.matrix <- function() x
  
  set.inverse.matrix <- function(solve)
  {
    inverse.matrix <<- solve
  }
  
  get.inverse.matrix <- function() inverse.matrix
  
  list(set.matrix = set.matrix, get.matrix = get.matrix, set.inverse.matrix = set.inverse.matrix, get.inverse.matrix = get.inverse.matrix)
}

## cacheSolve - This function calculates / retrieves the inverse matrix
cacheSolve <- function(x, ...) {
  a.matrix <- x$get.inverse.matrix()
  if(!is.null(a.matrix)) {
    message("Fetching the inverse matrix from cache")
    return(a.matrix)
  }
  data <- x$get.matrix()
  a.matrix <- solve(data, ...)
  x$set.inverse.matrix(a.matrix)
  a.matrix
}
