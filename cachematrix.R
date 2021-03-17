## Put comments here that give an overall description of what your
## functions do

## Main purpose of this experiment is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input, which is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve is a function which evaluates the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been evaluated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Check

> m <- matrix(rnorm(16),4,4)
> m <- matrix(rnorm(16),4,4)
> m1 <- makeCacheMatrix(m)
> cacheSolve(m1)
           [,1]        [,2]       [,3]       [,4]
[1,] -0.4969018  0.15234551 -0.6658803  0.9505499
[2,]  0.3174180  0.17832997  3.4070834  0.4256563
[3,]  0.1609753 -0.50071186 -0.1367826  0.7648571
[4,] -0.2745764  0.05778194  0.1071880 -0.5087613
