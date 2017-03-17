makeCacheMatrix<- function (stored.value = matrix()) {
        inverted <- NULL
        set <- function (value) {
                stored.value <<- value
                inverted <<- NULL
        }
        get <-function() stored.value
        getInverted <- function () {inverted }
        setInverted <- function (value) inverted <<- value
        list(set=set, get =get, getInverted = getInverted, setInverted=setInverted)
}

cacheSolve <- function(x, ...) {
        m <- x$getInverted()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get(), ...)
        x$setInverted(m)
        m
}