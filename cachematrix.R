
makeCacheMatrix <- function(x = matrix()) {#matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCache <- function(solve) m
        getCache <- function() m
        list(set = set, get = get,
            setCache = setCache,
           getCache = getCache)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getCache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setCache(m)
        m
}
