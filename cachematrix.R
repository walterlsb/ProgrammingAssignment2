################################################
## makeCacheMatrix() and cacheSolve() work together
## to cache matrix inversions


#################################################
## makeCacheMatrix
## Returns 
##      Function list:
##              - set(your_matrix here)                 : set the internal variable with your data matrix
##              - get()                                 : returns your_data_matrix
##              - setinverse(inverse_matrix here)       : sets the value of the inverse matrix - set from cacheSolve function
##              - getinverse()                          : returns the inverse of your_data_matrix once calculated by cacheSolve Function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize variable for storage of inverse
        inverse <- NULL
        
        
        ## define functions
        set <- function(matrixIn) {
                x <<- matrixIn
                inverse <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inv){
                inverse <<- inv
        }
        
        getinverse <- function() {
                inverse
        }
        

        ## returns list of functions
        list(
                set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


#################################################
## cacheSolve
## Arguments
##      - list created by makeCacheMatrix()
## Returns 
##      - inverse of matrix stored in the makeCacheMatrix() list
##      - also stores the inverse in makeCacheMatrix list

cacheSolve <- function(x, ...) {
        ## retrieve the inverse stored in makeCacheMatrix()
        cachedinverse <- x$getinverse()
        
        ## if that stored value is not null, return it and end
        if(!is.null(cachedinverse)) {
                message("getting cached data")
                return(cachedinverse)
        }
        
        ## if no stored value, get the stored matrix data and solve the inverse
        data <- x$get()
        newinverse <- solve(data, ...)

        ## set the stored inverse in makeCacheMatrix, return it and end
        x$setinverse(newinverse)
        newinverse
}

######################################################