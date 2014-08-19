# =============================================================================
# Programming Assignment 2
# Carlos Ruiz (cruizch@gmail.com)
# 2014-08-19
# =============================================================================

# =============================================================================
# makeCacheMatrix
# function that creates a cached inverse matrix object
#
# Carlos Ruiz (cruizch@gmail.com)
# 2014-08-19
# =============================================================================
# m     matrix object
# =============================================================================
makeCacheMatrix <- function(m = matrix()) {

    inv <- NULL
    
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    
    get <- function() {
        m
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

# =============================================================================
# cacheSolved
#
# function that calculates the matrix inverse (just the first time) or gets the
# cached matrix inverse
#
# Carlos Ruiz (cruizch@gmail.com)
# 2014-08-19
# =============================================================================
# m     cache matrix object
# =============================================================================
cacheSolve <- function(m, ...) {

    inv <- m$getInverse()
    
    if(!is.null(inv)) {
        message("get the cached matrix inverse")
        return(inv)
    }
    
    # for this assignment we assume that the matrix is invertible, but we can
    # use the ginv function of the MASS package to get a generalized inverse,
    # so if the matrix is invertible then solve(m) == ginv(v)
    
    inv <- solve(m$get(), ...)
    m$setInverse(inv)
    
    inv

}

# ============================================================================
# the following code is for test purposes
# Carlos Ruiz (cruizch@gmail.com)
# 2014-08-19
# ============================================================================

# test matrix
test <- matrix(data = c(1, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3, ncol = 3)

# construct a cache matrix object
myMatrix <- makeCacheMatrix(test)

# access to the current inverse, must return null
myMatrix$getInverse()

# calculate the matrix inverse
cacheSolve(myMatrix)

# access to the current inverse, must be not null
myMatrix$getInverse()

# must print cache access and return the cached matrix inverse
cacheSolve(myMatrix)
