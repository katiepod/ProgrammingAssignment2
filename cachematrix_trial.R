## Create a function for storing the output of the cacheSolve() fnx. 
## It will contain 3 functions that cacheSolve can access.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL # initiatize the inverse variable (empty)
        
        get <- function() { x } # this function returns the value of the original matrix
        
        setSolve <- function(solve)  { inverse <<- solve }
        # this is called by cacheSolve() during the first cacheSolve()
        #  access (i.e when inverse is NULL) and it will store the value using superassignment
        
        getSolve <- function() { inverse }
        # this will return the cached value to cacheSolve()
        
        list(get = get,          #  OK, this is accessed each time makeCacheMatrix() is called,       
             setSolve = setSolve,  #   that is, each time we make a new object.  This is a list of 
             getSolve = getSolve) # the internal functions ('methods') so a calling function
                                # knows how to access those methods.
}


## Create a calling function that will perform the matrix inversion, and if the 
## result has been previously calculated, return a cache

cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix fnx
        
        inverse <- x$getSolve() # accesses the object 'x' and gets the chached output
        
        if(!is.null(inverse)) { # if inverted matrix was already cached (not NULL) ...
                message("getting cached data") # ... send this message to the console
                return(inverse) # ... and return the inverted matrix; "return" ends the function cacheSolve()
        }
        data <- x$get() # if x$getmean() returned NULL we load up the data from makeCacheMatrix
        
        inverse <- solve(data, ...) # and invert the object x using the solve() fnx
        
        x$setSolve(inverse) # store the inverted matrix in x
        
        inverse # return the interveted matrix to the code that called this function
}
