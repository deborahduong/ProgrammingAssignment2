## Finds the inverse of a matrix, and at the same time caches the result
## so that the next time you call it, it doesnt recompute it.  Example:

##> aMatrix <-makeCacheMatrix(matrix(1:4,2,2))
##> cacheSolve(aMatrix)
##    [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(aMatrix)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
 

## first make the cached matrix

makeCacheMatrix <- function(x = matrix()) {


        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## call this function to find the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
		
}
