## https://github.com/philchang46/ProgrammingAssignment2.git
## 1st commit SHA-1 hash identifier: 
## R Programming Assignment 2: Lexical Scoping

## The makeCacheMatrix function creates a special "matrix", which
## which is a list containing a function to "set and get" the value
## of the matrix, and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() return(x)
        setcache <- function(cache) c <<- cache
        getcache <- function() c
        list(set = set, get = get, setcache = setcache, getcache = getcache)
  
}

## The cacheSolve function calculates the inverse of the special "matrix"
## which created with the makeCacheMatrix function. First, it checks if 
## the inverse value has already been calculated. If it has, the inverse 
## value is obtained. Otherwise, it calculates the inverse matrix and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return the value of matrix that is the inverse of x
    c <- x$getcache()
    if(!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    data <- x$get()
    c <- solve(data, ...)
        x$setcache(c)
        return(c)
}