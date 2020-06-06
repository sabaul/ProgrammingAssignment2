## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # set default inverse as null
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(mat_inv) inv <<- mat_inv
        get_inv <- function() inv
        list( set = set,
              get = get,
              set_inv = set_inv,
              get_inv = get_inv)

}


## Write a short comment describing this function

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        mat_rix <- x$get()
        inv <- solve(mat_rix)
        x$set_inv(inv)
        inv
}
