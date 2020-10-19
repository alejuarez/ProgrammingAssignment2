## This function assigns the inverse of the matrix for the first time
## and it saves it in a variable setInverse
makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    
    ## In here it assigns the value of the matrix to x
    
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    ## In here it stores the inverse value of the matrix
    
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## This function evaluates if getInverse is already calculated
## and displays the message "getting cache data"
## otherwise it calculates the inverse 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    ## got the result from makeCachematrix
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting cached data")
        return(j)
    }
    
    ## if it is not stored before it makes the inverse of the matrix
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}
