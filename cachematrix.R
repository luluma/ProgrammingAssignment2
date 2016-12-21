## R Programming - Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix
## This assignment contains two functions:
## 1. makeCacheMatrix
## 2. cacheSolve

## This function creates a special matrix that can cache its inverse, by doing:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y){
        x <<- y
        inv_x <<- NULL
    }
    get <- function(){
        x 
    }
    setinverse <- function(inv_matrix) { 
        inv_x <<- inv_matrix 
    }
    getinverse <- function() {
        inv_x
    }
    
    list(set = set, get = get,
         setinverse = setinvers,
         getinverse = getinverse)
}


## This function calculates the inverse of the "makeCacheMatrix"
## 1. if the inverse has already been calculated, it gets the inverse from cache
## 2. if not, it calculates the inverse and set it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_mtx <- x$getinverse()
    if(!is.null(inv_mtx)) {
        message("getting inverse from cache")
        return(inv_mtx)
    }
    org_mtx <- x$get()
    inv_mtx <- solve(org_mtx,...)
    x$setinverse(inv_mtx)
    inv_mtx
}
