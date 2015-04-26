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