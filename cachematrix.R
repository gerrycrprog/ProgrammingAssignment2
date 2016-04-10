## These two functions are designed to ensure that the inverse
## of a mtrix is not calculated more than once. They do this by 
## caching the inverse of the matrix the first time the inverse 
## is calcualted and callig the cache of the inverse if inverse
## is required again. 

## makecachematrix() creates a list which stores four functions that
## set and get a matrix and set and get the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        ## Define function setm() - Sets the Matrix    
        setm <- function(yt) {
                x <<- yt
                In <<- NULL
        }
        ## Define functions getm(), setinv(), getinv() : 
        ## getm()   - Gets the Matrix created by Set
        ## setinv() - Sets the Inverse of the Matrix
        ## getinv() - Gets Matrix Inverse set by Setinv
        getm <- function() x
        setinv <- function(solve) Inv <<- solve
        getinv <- function() Inv
  
        ## Function returns list containing the four functions Setm(),getm(), 
        ## setinv() and getinv() 
        list(setm = setm, getm = getm,
        setinv = setinv,
        getinv = getinv)
}


## This function checks if Inverse calculated previously. If inverse already 
## calcualted, it is retrieved from cache, otherwise inverse calculated 
## using solve function.

cacheSolve <- function(x, ...) {
  
        ## Cached Inverse retrieved using getinv(). If non NULL matrix cached inverse 
        ## returned. 
        Inv <- x$getinv()                    
        if(!is.null(Inv)) {
                message("Using cached inverse")
        return(Inv)
        }
        ## Matrix data retrieved using getm() and Inverse calculated using solve()
        ## and returned. Inverse also stored in Cache using setinv().
        data <- x$getm()
        Inv <- solve(data, ...)
        x$setinv(Inv)
        Inv
}