#' Creates a special list with keys "get", "set", "setsolve" and "getsolve" 
#' that will cache the result of inversing a matrix
#' 
#' You can use this object directly, but to be able to correctly cache
#' the inverse of the matrix you should always use the `cacheSolve` function
#' on it.
#' 
#' "get" will read give you the currently wrapped matrix
#' "set" will change the currently wrapped matrix and clear the inverse cache if there is one
#' "setsolve" will save the cached inverse, should not be directly called
#' "getsolve" will get the cached inverse, should not be directly called
#' 
#' @param x the matrix that will have it's inverse cached
#' @return a list wrapping the given matrix
#' @examples
#' m <- makeCacheMatrix(matrix( rnorm(N*M,mean=0,sd=1), 3, 3))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solved) m <<- solved
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


#' Calculates and caches or returns the cached result of inversing the given matrix
#' 
#' This function will either return the available inverse directly if it isn't cached
#' or will calculate the inverse, save it to the cache and then return this cached result.
#' 
#' @param x the special list created by `makeCacheMatrix`
#' @param ... other parameters you would usually give to a call to `solve`
#' @return the inverse of the currently wrapped matrix
#' @examples
#' m <- makeCacheMatrix(matrix( rnorm(N*M,mean=0,sd=1), 3, 3))
#' cacheSolve(m)

cacheSolve <- function(x, ...) {  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
}