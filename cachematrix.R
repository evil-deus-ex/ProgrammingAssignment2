## Cached calculation of the matrix inverse. makeCacheMatrix() - builder function to create the data container.
## cacheSolve - main function for calculating the inverse of the matrix

## create and return the data container for the given matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## create an empty variable for inverse
    set <- function(y) { ## declare matrix set function
        x <<- y ## save matrix 
        inv <<- NULL ## create an empty variable for inverse 
    }
    get <- function() { ## declare matrix get function
        x ## return given matrix    
    } 
    setinverse <- function(inverse) { ## declare set inverse cache function
        inv <<- inverse ## save inverse in the container    
    }
    getinverse <- function() { ## declare get inverse cache function
        inv ## return current inversed matrix or NULL if not set yet
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## create and return data container
}


## calculate the inverse of the matrix or if it have been already calculated - return the cached value
## 'x' should be the result of the makeCacheMatrix() function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() ## get current inverse cache value
    if (!is.null(inv)) { ## check did we already store the inverse
        if (dim(inv) == c(1,1) && is.na(inv[1,1])) { ## check for the uninversable matrix cache
            print("Matrix has no inverse") ## print message   
        }
        return(inv) ## if we have cache - return the value and exit the function
    }
    data <- x$get() ## get the given matrix
    error <- function(e) { ## declare error handler function
        print("Matrix has no inverse") ## print message
        matrix() ## return empty matrix
    }
    inv <- tryCatch(solve(data), error = error) ## safety calculate the inverse
    x$setinverse(inv) ## save the inverse in the data container
    inv ## Return a matrix that is the inverse of 'x'
}
