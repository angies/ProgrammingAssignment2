## Caching the Inverse of a Matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.


## Create a special "matrix" object that can cache its inverse
## This function contains a list of functions (set,get,setsolve,getsolve)
makeCacheMatrix <- function(x = matrix()) {
   im <- NULL
   
   ## set the value of the matrix
   set <- function(y) {
      x <<- y
      im <<- NULL
   }
   
   ## get the value of the matrix
   get <- function() x
   
   ## set the value of the inverse matrix
   setsolve <- function(solve) im <<- solve
   
   ## get the value of the inverse matrix
   getsolve <- function() im
   
   ## set a list of functions
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix above
## NOTE: assume that the matrix supplied is always invertible
cacheSolve <- function(x, ...) {  
   im <- x$getsolve()
   
   ## If the inverse has already been calculated (and the matrix has not changed), 
   ## then the cacheSolve should retrieve the inverse from the cache.
   if(!is.null(im)) {
      message("getting cached data")
      return(im)
   }
   
   ## Else the inverse matrix is calculated
   data <- x$get()
   im <- solve(data, ...) 
   x$setsolve(im)
   im       ## Return a matrix that is the inverse of 'x'
} 



