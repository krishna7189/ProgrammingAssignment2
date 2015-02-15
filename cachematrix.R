## Caching the Inverse of a Matrix



##These two functions provide the following benefits:-
## 1. If a matrix is unchanged and its inverse is already computed; then cached inverse is returned
## 2. If a matrix is unchanged and its inver is not computed before running "cachesolve", then for the first time 
##    inverse is computed and following times, cached inverse is returned
## 3. If a matrix is changed, Then step 1 or 2 shall be performed.



## "makeCacheMatrix" function can be used to set a matrix whose inverse can be computed later OR can be used to set a
## matrix along with its inverse. this inverse is stored under global environement, which can be cached later by
## any other function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y)
      {
    x <<- y
    inv <<- NULL
  }
  
  getmatrix <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## "cacheSolve"function is used to retrieve already computed inverse of a given matrix from cache or computes the 
## inverse of a new matrix and stores it back into cache memory.

cacheSolve <- function(x, ...) {
 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("Computing Inverse of a matrix")  
  inv <- ginv(x$getmatrix())
  x$setinverse(inv)
  inv
}

# Some Examples (run in order to uderstand the functionality of the above two functions better)
# a<- matrix(1:4,2,2)
# b<- makeCacheMatrix(a)
# b$setmatrix()
# b$getmatrix()
# cacheSolve(b)
# cacheSolve(b)
# b$setmatrix(matrix(1:10,2,5))
# b$getmatrix()
# b$setinverse(ginv(b$getmatrix()))
# b$getinverse()
# cacheSolve(b)
# b$setmatrix(matrix(1:8,2,4))
# cacheSolve(b)
# cacheSolve(b)
