## There are two functions, makeCacheMatrix() and cacheSolve(). 
## makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
## cacheSolve() takes an argument that is returned from makeCacheMatrix(), which is the cached inverse value stored in makeCacheMatrix() environment (if it exists).

## First, two objects are initialised, x and inverse. inverse is set to NULL. 
## set() function inside the makeCacheMatrix assigns the input to the x in the parent environment and assigns NULL to the inverse, which clears any cached inverse. 
## Simple get() function follows, together with setinverse() and getinverse(). Lastly, the list is returned to allow us to use $ sign.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function makes or retrieves the inverse from a makeCacheMatrix() object. First, it calls the getinverse() on the input argument and checks if the result is NULL.
## If there is inverse value, then it returns it to the parent environemnt of the function cacheSolve().
## If inverse value is NULL, then the matrix is called from the makeCacheMatrix() with get(), inverse is calculated with solve() and setinverse() is used to set the inverse in the makeCacheMatrix.
## Finally, it returns the value of the inverse value.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}