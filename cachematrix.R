## Inversing a matrix is a heavy computation. These two functions aim to cache
## the inverse of given matrix so that it can be used later without further 
## computation

## this functions returns a list a setter and getter of a matrix and a setter
## and a getter of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_of_x <- NULL
    set <- function(y){
        x <<- y
        inverse_of_x <<- matrix()
    }
    get <- function(){
        x
    }
    set_inverse <- function(inverse) inverse_of_x <<-- inverse
    get_inverse <- function() inverse_of_x
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## function to compute the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_of_x <- x$get_inverse()
    if(!is.null(inverse_of_x)){
        return(inverse_of_x)
    }
    mat <- x$get()
    inverse_of_x <- solve(mat, ...)
    x$set_inverse(inverse_of_x)
    inverse_of_x
}
