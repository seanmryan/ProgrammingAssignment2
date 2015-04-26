## The following functions are designed to create a matrix object that can cache it's 
## inverse, and retreive the inverse from the cache (or create it's own if it can't 
## find it), respectively.

## The makeCacheMatrix function is a function that stores a list of functions.
## get(): return the matrix described in your argument
## set(): overwrite the value of the matrix with another matrix.
## setinv(): set the value of the inverse matrix.
## getinv(): returns the inverse you set
## It creates a blank inverse by default which you can set later. If you set a new matrix the inverse is
## You can create a special object through which you can call these functions (e.g., x$get() ).


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y,r,c) {
            x <<- matrix(y,r,c)
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The cacheSolve function makes use of the functions in makeCacheMatrix 
## to generate the inverse of a matrix created with said function. It will
## use getinv() to retrieve the inverse value set in the special object created and
## return it if the inv value is not NULL. If inv is NULL, it will get the cached matrix, get
## the inverse of the matrix itself, set it as the new inverse and then return it.

#cacheSolve Function
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}

