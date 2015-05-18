## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(A = matrix()) {#caches the inverse of a matrix
  Inv <- NULL #sets inverse to NULL
  set <- function(y) { #sets the matrix
    A <<- y
    Inv <<- NULL
  }
  get <- function() A #gets the matrix
  setinverse <- function(inv) Inv <<- inv  #caches the inverse of the matrix determined by the cacheSolve function
  getinverse <- function() Inv #gets the inverse from cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 I <- x$getinverse() #gets the inverse from cache
  
 if(!is.null(I)) { #checks if cache is not empty
    print("getting cached data")
    return(I) #returns the value stored in cache
  }
  data1 <- x$get() #gets the matrix to be inverted
 print(data1) #prints the matrix
  I <- solve(data1) #inverts the matrix
  x$setinverse(I) #sets the inverse, which will be stored in cache
  I #returns the inverse matrix

}
