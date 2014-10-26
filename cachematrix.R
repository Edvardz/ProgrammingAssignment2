## The functions makeCacheMatrix and cacheSolve are used to avoid recalculating the inverse of a matrix that has already been solved.
## To make use of the cache functionality, first cache a matrix with makeCacheMatrix, and then call the cacheSolve function to invert it
## The cacheSolve function will only compute the inverse of the matrix the first time it is called, and use the cached value thereafter


makeCacheMatrix <- function(theMatrix = matrix()) {
    # This function stores a matrix, and has the following methods:
    #
    # set         Stores a new matrix, overwriting the old one (and clears the stored inverse)
    # get         Returns the stored matrix
    # setInverse  Stores the inverse of the matrix
    # getInverse  Returns  the inverse of the matrix
    #
    # the function also to store an inverse of the matrix (computed elsewhere), and to get the inverse of the matrix
    
    theInverseMatrix <- NULL                                      # Holds the inverse matrix

    set <- function(newMatrix) {                                  # Stores a new matrix (overwrites the old one)
        theMatrix <<- newMatrix                                   # Store the matrix
        theInverseMatrix <<- NULL                                 # clear old stored inverse
    }

    get <- function() theMatrix                                   # Returns the stored matrix
    
    setInverse <- function(inverse) theInverseMatrix <<- inverse  # Stores the inverse matrix
    
    getInverse <- function() theInverseMatrix                     # Returns the stored inverse matrix
    
    list(set = set,                                               # The methods of the object
         get = get,                                                
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(theMatrix, ...) {
    # This function inverses a matrix
    # To avoid unecessary computation, it first checks whether the inverse has already been calculated
    
    theInverseMatrix <- theMatrix$getInverse()                    # Try to retrive the stored inverse of the matrix
    if(!is.null(theInverseMatrix)) {                              # If there was indeed a stored inverse then
        message("getting cached inverted matrix")                 #   - Alert the user
        return(theInverseMatrix)                                  #   - and return the stored inverse (exiting the function)
    }
    data <- theMatrix$get()                                       # No inverse was stored, so retrieve the matrix
    theInverseMatrix <- solve(data, ...)                          # Inverse the matrix using the solve() function
    theMatrix$setInverse(theInverseMatrix)                        # Store the inverse matrix
    theInverseMatrix                                              # Return the newly calculated inverse
}

