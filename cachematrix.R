##Author: Marc Carmen
##
##  This file contains two functions makeCacheMatrix and cacheSolve. 
##  Detailed descriptions of each function are in the comments for the respective function
##
##  Usage:
##      > cached <- makeCacheMatrix()
##      > cached$set(matrix(1:4, 2, 2))
##      > cached$get()
##      [,1] [,2]
##      [1,]    1    3
##      [2,]    2    4
##      > cached$getInverse()
##      NULL
##      > cacheSolve(cached)
##      [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > cached$getInverse()
##      [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > cacheSolve(cached)
##      getting cached data
##      [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > cached$get() %*% cached$getInverse()
##      [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1

##  makeCacheMatrix(x = matrix())
##
##  This function creates a data structure with
##      - x:                    the original matrix (initialized using the function argument)
##      - inverseX:             the inverse of the matrix (initialized at NULL)
##  and a set of function for accessing that data
##      - set(y):               Sets the value of X to the argument and reset inverse to NULL
##      - get():                Return the original matrix
##      - setInverse(inverse):  Set the inverseX to the argument for the function
##      - getInverse():         Return the inverse of the matrix
## 
##
##  params
##      x   Defaults to a blank matrix 
##
##  returns
##      List of functions (set, get, setInverse, getInverse) accessible 
##      through the data type that is created
##         
makeCacheMatrix <- function(x = matrix()) {
    x <- x
    inverseX <- NULL
    set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseX <<- inverse
    getInverse <- function() inverseX
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##  cacheSolve(x, ...)
##
##  This function expects to receive a matrix cache data structure created using 
##  makeCacheMatrix.  If any other data structure is used as an argument there will be an error.
##  If the argument (x) already has an inverse then the cached inverse is returned otherwise
##  the inverse is created and stored.
##
##  If the inverse has previously been cached then a message is output notifying the user
##  that the cached data is being used.
##
##  params
##      x   The data type created using makeCacheMatrix
##      ... Any other parameters that will be passed on to the solve function
##
##  returns
##      A matrix that is the inverse of the initial matrix
##  
cacheSolve <- function(x, ...) {
    inverseX <- x$getInverse()
    if(!is.null(inverseX)) {
        message("getting cached data")
        return(inverseX)
    }
    data <- x$get()
    inverseX <- solve(data, ...)
    x$setInverse(inverseX)
    inverseX
}
