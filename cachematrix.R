## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 'makeCacheMatrix' creates a list that contains the functions to handle 
## the cache of the original matrix ('set' and 'get' functions)   
## and its invers ('set_inv' and 'get_inv').
## You have to istantiate the cache first before using the function 
## 'cacheSolve'. Find hereafter two examples of how to use
## 'makeCacheMatrix':
##
## a <- matrix(rnorm(16), 4, 4) ## 'a' is the original matrix
## b <- makeCacheMatrix() ## instantiates the cache in the list 'b'
## b$set(a) ## sets the cache with the value of the matrix 'a' or...
##
## b <- makeCacheMatrix(a) ## do the same but in a single line
##
## Then the next step is to use 'cacheSolve' to calculate the invers
## of 'a' if the invers is not yet available in the cache, or to get it
## from the cache if the invers is already available.
## To do it use the following command
##
## c <- cacheSolve(b) ## 'c' contains the invers of 'a'
##
## You can check it by executing the following calculation d <- c %*% a
## you will get 'd' a matrix which is very close to the identity matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    set_inv <- function(matrix_inv) inv_matrix <<- matrix_inv
    get_inv <- function() inv_matrix
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ##print(x)
    inv_matrix <- x$get_inv()
    if(!is.null(inv_matrix)) {
        message("getting cached inv_matrix")
        return(inv_matrix)
    }
    matrix_data <- x$get()
    inv_matrix <- solve(matrix_data)
    x$set_inv(inv_matrix)
    inv_matrix
}
