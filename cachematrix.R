## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.
## The two functions below are used to create a special object that stores a matrix
## and caches its inverse.


## The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to:
## 1 -> set the value of the matrix
## 2 -> get the value of the matrix
## 3 -> set the value of the inverse
## 4 -> get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The second function, cacheSolve calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Else, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
