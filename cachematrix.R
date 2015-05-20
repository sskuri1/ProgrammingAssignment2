## there are two functions - 1. called makeCacheMatrix that creates a special 
##"matrix" object that can cache its inverse and 2. called cacheSolve that 
##computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve will retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the matrix
##get the matrix
##set the solve function
##get the solve function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL # sets the value of m to NULL to provide default if cacheSolve has not been used
  set <- function(y) {
    x <<- y ## within the setmatrix function caches the input matrix so that cacheSolve can check whether it has changed 
    m <<- NULL ## if cacheSolve is used sets the value of m (the matrix inverse) to NULL
  }
  get <- function() x # returns the stored matrix
  setmatrix <- function(solve) m <<- solve # cache the given argument 
  getmatrix <- function() m  # get the cached value
  list(set = set, get = get,  # return a list. Each named element of the list is a function (as defined above)
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This SECOND function inverses the special matrix created with the above function by first 
##checking to see if the inverse has already been created. If so, it gets the inverse matrix 
##from the cache and skips the computation. Otherwise, it creates the inverse matrix and sets 
##the value of the solve in the cache via the setmatrix function.

cacheSolve <- function(x=matrix(), ...) { #compare matrix to what was there before
        m<-x$getmatrix() # if an inverse has already been calculated get it from cache
  if(!is.null(m)){# check if cacheSolve was run earlier
    message("getting cached data")
        return(m)# get inverse from existing cache
  }
  #if matrix is new, get the matrix, caclulate the inverse and store it in cache
  matrix<-x$get()# get the new matrix
  m<-solve(matrix, ...)# calculate inverse
  x$setmatrix(m)#cache inverse
  m #return inverse
}