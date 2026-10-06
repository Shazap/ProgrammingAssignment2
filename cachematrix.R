## Put comments here that give an overall description of what your
## functions do

## Creating makeCacheMatrix closure which can take matrix input to create an object that will have set(), get(), setinv() & getinv() as functions to utilise.
## It uses cache technic to check if the result is already available or not to save redundant processing

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This is function is to calculate the inverse of a given matrix along with active cache checking to limit redundant processing for existing result for
## already existing matrix.
cacheSolve <- function(x){
  inv <- x$getinv()
  
  if(is.matrix(inv)){
    print('Inverse is already present')
    return(inv)
  }
  mtr <- x$get()
  inv <- solve(mtr)
  x$setinv(inv)
  
  inv
}
