## makeCachematrix will first create a matrix and set the value of the new
## matrix as well as the value of the inverse of the matrix.  It will also
## create a get for the matrix value and the matrix inverse value.  The
## second function will check to see if the inverse of the new matrix is in
## cache and, if so, retrieves that value.  Otherwise, the second function will
## calculate the inverse of the new matrix.

## Create a matrix object.  Set values of matrix and matrix inverse.  Create
## gets for matrix and matrix inverse values.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Check for matrix inverse value in cache.  If there, retrieve value. If not
## calculate matrix inverse value.  Return matrix inverse value.x

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
