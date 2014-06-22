## Caches an inversed matrix

## Creates a matrix that can be inversed
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setMatrixInverse <- function(inverse) {
        inv <<- inverse
    }
    getMatrixInverse <- function() {
        inv  
    } 
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Inverses the matrix and caches the result
cacheSolve <- function(c, ...) {
    ## Returns a matrix that is the inverse of 'x'
    cInv <- c$getMatrixInverse()
    if(!is.null(cInv)) {
        message("getting cached inversed matrix")
        return(cInv)
    }
    data <- c$get()
    cInv <- solve(data) %*% data
    c$setMatrixInverse(cInv)
    cInv 
}

## Run the following code to initialize the functions
## v = matrix(c(1, 2, 3, 4), 2)
## cv = makeCacheMatrix(v)
## cacheSolve(cv)
## cacheSolve(cv)