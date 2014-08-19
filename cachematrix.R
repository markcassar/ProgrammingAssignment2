## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function returns a list of functions that allows you to:
# - set the value of a matrix
# - get the value of a matrix
# - set the value of the inverse of a matrix
# - get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      # create a NULL matrix inverse
      matrix_inv <- NULL #, nrow=0, ncol=0)
      # create a function that allows you to set the value of a matrix
      set <- function(y) {
            x <<- y
            matrix_inv <<- NULL #, nrow=0, ncol=0)
      }
      # a function that allows you to get the value of a matrix
      get <- function() x
      # a function that allows you to set the value of the inverse
      setinverse <- function(inverse) matrix_inv <<- inverse
      # a function that allows you to get the value of the inverse
      getinverse <- function() matrix_inv
      # returns a list of these 4 functions
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This function checks to see if the inverse of a given matrix x
# has already been calculated. If it has, it will return the value
# of the inverse from the cache instead of calculating it again. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if the inverse has been calculated already
        matrix_inv <- x$getinverse()
        # if 'matrix_inv' has been calculated its value will be NON-NULL
        #     and the inverse will be return.
        # if 'matrix_inv' has not been calculated its value will be NULL
        #     and the inverse will be calculated
        if(!is.null(matrix_inv)) {
                message("getting cached data for matrix inverse")
                return(matrix_inv)
        }
        # set data to the value of the matrix
        data <- x$get()
        # calculate the inverse of this matrix using 'solve'
        matrix_inv <- solve(data,...)
        # once the inverse is calculated (which it will as we are assuming
        # all our matrices are invertible) use the setinverse function to 
        # store the value of this matix's inverse
        x$setinverse(matrix_inv)
}
