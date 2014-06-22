## these two functions are used to cache the inverse of a matrix rather than compute it repeatly.

## the first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse
             getinverse = getinverse)

}


## the second function calculates the inverse of a the special "matrix" created with the above function. It first
##checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache
## via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
