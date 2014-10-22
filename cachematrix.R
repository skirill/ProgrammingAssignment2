## The file contains a pair of functions allowing to cache the inverse matrix
## for a given square matrix. Whenever you call cacheSolve (which is a variant
## of solve that supports caching), it checks whether inverse has aready been
## calculated and returns the cached copy if so, calculating it anew oherwise.

###############################################################################
## makeCacheMatrix - creates a special 'object' representing the matrix with
## its cached inverse that may be manipulated through the API:
##
## get() - get the matrix
## set() - set the matric and clear the inverse (don't auto calculate it!)
## getInverse() - get the inverse, possibly NULL
## setInverse() - set the inverse (cache it)
##
## Actual calculation of the inverse takes place outside, this object is just
## caching the result.

makeCacheMatrix <- function(mtrx = matrix()) 
{
    ## This function will contain mtrx and inverse in its environment
    ## Due to lexical scoping rules of R, all functions defined here will
    ## work with these objects, so they represent a 'state' that stores
    ## cached data for a list object that is returned by this function.
    
    # Initially there is no cached inverse
    inverse <- NULL
    
    # Also it is reset when we set a new matrix
    set <- function(newMtrx) 
    {
        # TBD: check that mtrx is a square matrix. Accoring to the task
        # definition, it is assumed to be invertible
        mtrx <<- newMtrx
        inverse <<- NULL
    }
    
    # Getter just takes the matrix from the environment attached to the 
    ## cached matrix object
    get <- function() mtrx
    
    ## Called when inverse was calculated on demand to store it in a state
    setInverse <- function(newInverse) inverse <<- newInverse
    
    ## Just return inverse from state, maybe NULL
    getInverse <- function() inverse
    
    ## Here we bundle all four methods in a list that is returned. Now one
    ## can use this list with the net function and it will carry an environment
    ## where data is cached
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


###############################################################################
## cacheSolve - return an inverse for the first argument. Performs as solve, 
## but supports caching. The first argument should be a special "cached matrix"
## object (actually a list with named items) created using makeCacheMatrix
## You should create it once and pass to cacheSolve whenever you need an
## inverse. When your matrix to inverse changes, you should call $set() 
## function of the object (this will reset the inverse). For the cache to be 
## useful, you should set the matrix more seldom than solving for its inverse
## of course.

cacheSolve <- function(mtrx, ...) 
{
    ## Return a matrix that is the inverse of 'mtrx'
    
    # See if we have a cached inverse
    inverse <- x$getInverse()
    if(!is.null(inverse)) 
    {
        message("getting cached data")
        return(inverse)
    }
    # We're here so no we don't -- calculate it
    data <- x$get()
    inverse <- solve(data, ...)
    # Cache it and then return
    x$setInverse(inverse)
    inverse
}
