## makeCacheVector creates a list containing a functions to:
## - set the value of the vector
## - get the value of the vector
## - set the value of the mean
## - get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve calculates the inverse of the cachable matrix created by 
##makeCacheMatrix. 
##It starts by checking whether an inverse already exists, and returns 
##it in the affirmative.
##Otherwise, it calculates the inverse, caches it via setinverse and 
##returns the value.
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
        ## Return a matrix that is the inverse of 'x'
}
