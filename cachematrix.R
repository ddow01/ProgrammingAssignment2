## These functions will 1) create a matrix object that can cach its inverse
## 2) compute the inverse of the matrix if needed
## If the inverse has already been computed, return the invers

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inverse <- function(x, ...) {return (solve(x)) }
        m <- inverse(data)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}


# testing
# https://class.coursera.org/rprog-016/forum/thread?thread_id=140
# source('U:/lo_pri/DS_R/CacheMatrix/cachematrix.R')
# test <- matrix(c(-1, -2, 1, 1), 2, 2)
# test1 <- makeCacheMatrix(-1, -2, 1, 1))
# Repeat to confirm caches
# test2 <- cacheSolve(test1)

