## compute the inverse of a matrix and cache the inverse

## makeCacheMatrix function can set,get the value of a matrix, 
## and can set,get the value of the inverse
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        # set matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # get matrix
        get <- function() x
        # set the inverse
        setinv <- function(inverse) inv <<- inverse
        # get the inverse
        getinv <- function() inv
        # returns a list of containing functions to set/get matrix/inverse
        list(set = set, get = get
             setinv = setinv,
             getinv = getinv
        )
}

## the cacheSolve function can calculate and return the inverse of a matrix if 
## the inverse is not cached, if the inverse is already computed then it returns 
## directly the cached inverse.

cacheSolve <- function(x, ...){
        # get inverse from cache
        inv <- x$getinv()
        # if the inverse is already cached,return the cached inverse
        if(!is.null(inv)){
                message("getting cached inverse for the matrix")
                return(inv)
        }
        # if not cached, then calculate the inverse of the matrix
        matrix_data <- x$get()
        inv <- solve(matrix_data)
        x$setinv(inv)
        inv
}
