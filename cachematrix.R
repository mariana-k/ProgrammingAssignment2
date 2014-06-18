## Caches an inversed matrix

## Creates a matrix that can be inversed

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setMatrixInverse <- function(inverse) {
        m <<- inverse
    }
    getMatrixInverse <- function() {
        m  
    } 
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Inverses a matrix and caches the result

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m <- x$getMatrixInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setMatrixInverse(m)
    m 
}
