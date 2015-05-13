## makeCacheMatrix accepts a matrix as an argument. It is assumed that the matrix is invertable. It returns a vector of lists. 

makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL

        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x

        setInverse <- function(inverse) Inv <<- inverse

        getInverse <- function() Inv

# Function return
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve inverts the matrix and stores result in cache. If the matrix is unchanged, the cached result is returned. If not, the inverse is computed.

cacheSolve <- function(x, ...) {

        Inv <- x$getInverse()
# Check cache
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }

        data <- x$get() # get matrix

        Inv <- solve(data, ...) #invert matrix

        x$setInverse(Inv) #new inverse

#function return
        Inv
}
