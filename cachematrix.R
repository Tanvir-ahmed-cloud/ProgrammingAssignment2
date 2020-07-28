## Put comments here that give an overall description of what your
## functions do
##There are two functions makeCacheMatrix and cacheSolve.
##First one creates a special matrix object that can cache its inverse and
##the second one computes the inverse of the special matrix returned by the first one.  
##If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
##makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #Function to get a matrix x. 
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv #Function to get the inverse.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Write a short comment describing this function
##It is used to cache the data.
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)) #checking whether the inverse is NULL. 
    {
    message("getting cached data")
    return(inv) #returns inverse value.
  }
  data <- x$get()
  inv <- solve(data, ...) #calculates the inverse value.
  x$setInverse(inv)
  inv #Return a matrix that is the inverse of x.
}
