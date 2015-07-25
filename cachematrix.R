## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix retrieves the previously computed inverse of  matrix  x and 
## stores the inverse for subsequent retrieval to avoid recomputation when nothing has changed.

## makeCacheMatrix turns an invertible numeric matrix into a list of 4 functions:
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse 
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## cacheSolve retrieves the inverse of the special object from the cache
## If cacheSolve can't find it, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)){
    message("getting cached data")
    return(invs) 
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
  
  ## Return a matrix that is the inverse of 'x'
}
