## Creates an inverted matrix from a given matrix and stores it in cache for 
## later much faster access. If inverted matrix is not available, it will be 
## calculated, cached and returned.

## Creates functionalities for later use.
## set = sets value of matrix 
## get = gets value of matrix
## setinverse = sets value of inverted matrix 
## getinverse = gets value of inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){x}
    setinverse <- function(inverse){inv <<- inverse}
    getinverse <- function(){ inv}
    list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## If inverted matrix is already in cache, function returns inverted matrix 
## from cache; otherwise calculates inverse matrix and returns it
cacheSolve <- function(x, ...) {
    ## Get inverted matrix. If data is available, return it.
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data...")
        return(inv)
    }
    #Otherwise: calculates, stores in cache and returns data
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    return(inv)
}

##Test functions above
matrix <- rbind(c(1, 5), c(1, 3))
t = makeCacheMatrix(matrix)
cacheSolve(t)
#Now from cache
cacheSolve(t)

