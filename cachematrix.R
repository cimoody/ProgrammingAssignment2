## Functions to set, store, retrieve, and evaluate the matrix and calculates 
## the inverse matrix

## This function creates a list that 
# 1. sets a value to a matrix
# 2. gets the value of the matrix
# 3. sets the inverse of the matrix
# 4. gets the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    # This function creates a list that 
    # 1. sets a value to a matrix
    # 2. gets the value of the matrix
    # 3. sets the inverse of the matrix
    # 4. gets the value of the inverse matrix
    inv <- NULL;
    set <- function(y) {
        x <<- y;
        inv <<- NULL;
    }
    get <- function() x;
    setinv <- function(solve) inv <<- solve;
    getinv <- function() inv ;
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv);
}


## This function returns the inverse of of a matrix 'x' either by calculating
## the inverse with solve(x) and caching it or returning a cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## either by calculating the inverse with solve
    ## or retrieving the cached valued.
    inv <- x$getinv();
    if(!is.null(inv)) {
        message("getting cached data");
        return(inv);
    }
    data <- x$get();
    inv <- solve(data, ...);
    x$setinv(inv);
    inv;
}