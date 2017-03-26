######## Programming Assignment 2: Lexical Scoping #########



###############################################################
########## -------- Building The functions -------- ###########
###############################################################

# Function to Chache values of Inverse Matrix:
## 

## Matrix inversion usually a costly computation and there may benefit to caching the
## inverse of a matrix rather than compute it repeatedly.
## You can see below the pair of functions that cache the inverse of a matrix.
## In other words, this pair of functions will create a object that contain a matrix caches its inverse.


## First, let's use the function that creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## For compute the inverse of the "matrix", created by function makeCacheMatrix,
## we'll use the function cachesolve. If the inverse has already been calculed (matrix
## has not chage), that it could retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}



###############################################################
######### --------- Testing the functions ---------- ##########
###############################################################
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()

test_matrix$getinverse()

cacheSolve(test_matrix)


test_matrix$getinverse()

test_matrix$set(matrix(c(3, 3, 2, 5), 2, 2))
test_matrix$get()

test_matrix$getinverse()

cacheSolve(my_matrix)



