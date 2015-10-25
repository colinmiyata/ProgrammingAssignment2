## The following functions allow for the inverse of a matrix to be calculated,
## cached and retrieved.

## makeCacheMatrix stores the matrix and the cached inverse in a list of 
## functions: get allows the matrix to be retrieved, set allows the matrix to be
## changed, and setSolve and getSolve are employed to store the inverse.

makeCacheMatrix <- function(x = matrix()) {

        s<-NULL
        set <- function(y=matrix()) {
                x<<-y
                s<<-NULL
        }
        get <- function() x
        setSolve <- function(sol) s <<- sol
        getSolve <- function() s
        list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
            
}


## cacheSolve checks if the matrix inverse is cached and will return the cached 
## value if available. If not the inverse is computed.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
    
}
