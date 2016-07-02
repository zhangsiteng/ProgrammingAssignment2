## Creat an object to store a matrix and cache the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
         x <<- y
         invrs <<- NULL
      }
     get <- function() x
     setinverse <- function(inverse) invrs <<- inverse
     getinverse <- function() invrs
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Below function cacluate the inverse of above matrix
## First check if the inverse of that maxtrix is already there ,if so ,returns the cached matrix. 
## Otherwise, it calculates the inverse of the matrix and set it in the cache. then returns it.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
     if(!is.null(invrs)) {
         message("getting cached data")
         return(invrs)
     }
     data <- x$get()
     invrs <- solve(data, ...)
     x$setinverse(invrs)
     invrs
}

