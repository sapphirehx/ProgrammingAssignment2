## compute the inverse of a matrix and cache the inverse

## makeCacheMatrix function can set,get the value of a matrix, 
## and can set,get the value of the inverse
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        # inverse
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # set the matrix
        get <- function() x
        # get the matrix
        setinv <- function(inverse) inv <<- inverse
        # set the inverse
        getinv <- function() inv
        # get the inverse
        list(set = set, get = get
             setinv = setinv,
             getinv = getinv
        )
}

## the cacheSolve function can calculate and return the inverse of a matrix if 
## the inverse is not cached, if the inverse is already computed then it returns 
## directly the cached inverse.

cacheSolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse for the matrix")
                return(inv)
        }
        # if inverse has been calculated
        matrix_data <- x$get()
        inv <- solve(matrix_data)
        x$setinv(inv)
        inv
}
