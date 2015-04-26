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