#These two function create, store and recall a matrix and its inverse in/from cache 

## This function creates a special "matrix" object,which is really a list 
#containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of a matrix



makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve take the special matrix object created by the makeCacheMatrix 
##function and calculates the inverse matrix of it
## but first it checks to see if the calculation has been done before
## if it has been calculated before it gets the data from the cache and skips the 
#computation. 
##If it has not been done 
## before it calculates the inverse matrix then set the value of the inverse
##matrixit in the cache via the setinverse function
  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
