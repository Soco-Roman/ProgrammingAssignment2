## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## value of the matrix
  a <- NULL
  set <- function(y){
    x <<-y 
    a <<- NULL
  }
  ## get the value of the matrix
  get <- function()x
  ## set the value of the inverse matrix
  s.inverse <- function(inverse) a <<- inverse
  ## get the value of the inverse matrix
  g.inverse <- function() a
  # save the values of the previous functions
  list(set = set, get= get,  setinverse = s.inverse, getinverse = g.inverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the generated matrix. If the inverse is already calculated, then this 
## function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## get the inverse
  a <- x$getinverse()
  ## if it's already calculated, then it will take de cache
  if(!is.null(a)){
    message("It is the cache data")
    return(a)
  }
  ## if not, it will calculate it.
  temp <- x$get()
  a <- solve(temp)
  # calculate the| inverse
  x$setinverse(a)
  a
}