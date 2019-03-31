## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special object for a given matrix x that has 
# a list of functions to set its value, get its value, set its inverse value, and get its inverse
# value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL         #create variable "inverse"
    set <- function(y){     #initialize function "set"
        x <<- y             
        inverse <<- NULL    #if x is changed, nullify the inverse
    }
    get <- function() x
    setInverse <- function(newInv) inverse <- newInv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve returns the inverse of a given matrix. If already computed, it prints the data,
# if not, it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()       #set "inverse" matrix from the makeCacheMatrix if available
    if(!is.null(inverse)){          #if inverse is already calculated, print it
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)     #compute the inverse matrix of the object makeCacheMatrix(x)
    x$setInverse(inverse)
    inverse
    
}


#Programming Assignment 2