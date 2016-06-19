###########################################################################################
##      OVERVIEW:
###########################################################################################

## As per assignment examples, first function creates a list with a function to:
## 1) Set matrix value
## 2) Get matrix value
## 3) Set inverse matrix
## 4) Get inverse matrix

## Second function is set up to either bring up the inverse matrix value (if cached)
## or to calculate it, if not found in cache.

###########################################################################################
##      FIRST FUNCTION:
###########################################################################################

## 'makeCacheMatrix' is as indicated in assignment, adjusted to consider 
##  matrix-type object instead of vector-type object.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()  x
        setinverse <- function(inverse)  m <<- inverse
        getinverse <- function()  m
        list(set = set , get = get,  setinverse = setinverse,  getinverse = getinverse)
        }

###########################################################################################
##      SECOND FUNCTION:
###########################################################################################

## 'cacheSolve' is as indicated in assignment, adjusted to consider
##  calculation of inverse-matrix rather than numeric mean.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
                
        ## Return a matrix that is the inverse of 'x'



###########################################################################################
##      EXAMPLE OUTPUTS:
###########################################################################################

## Test results with invertible 2x2 matrix
x <- matrix(1:4 , 2 , 2)
a <- makeCacheMatrix(x)
cacheSolve(a)

## Same test can be applied with indefinite number of invertible matrices, e.g.:
x <- matrix(c(1,4,5,7,3,7,9,4,7) , 3 , 3)
a <- makeCacheMatrix(x)
cacheSolve(a)


######################################################################################
##      ADDITIONAL NOTES

##      In order for the function to work, cacheSolve() requires an input-object of type makeCacheMatrix()
##      That explains the format of the outputs shown above.
##      
##      (please refer to mentor Leonard Greski's comment in the 3rd weeks's discussion forum: 
##      https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/bU_CQjSPEea8sQoZSoJycw)
##      for more information 


