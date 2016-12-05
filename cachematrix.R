## The following function creates a list of functions that 
## set, get, set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
                	x <<- y
                	inv <<- NULL
        }
	get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The following function returns the inverse of a matrix. It first checks
## to see if the inversematrix is cached, if so it returns the invere matrix else it calculates 
## the inverse of the matrix and sets the inverse of the matrix in the 
## cache by using setinvmatrix function

cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv

}
