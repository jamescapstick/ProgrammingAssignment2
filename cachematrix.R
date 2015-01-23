##a set of functions used to calculate and cache the inverse of a matrix

##create a special matrix object that can cache its inverse, and returns a set 
##of functions allowing us to cache the calculated inverse of the matrix
##inputs:
##x - the matrix we want to calculate the inverse of
makeCacheMatrix <- function(x = matrix())
{
    ##inverse is not cached yet
    i <- NULL
    
    ##set function for the matrix we're calculating the inverse of
    set <- function(y)
    {
        x <<- y
        ##make sure we clear the cache if we have a new matrix
        i <<- NULL
    }
    
    ##get function - returns the matrix we're calculating the inverse of
    get <- function() x
    
    ##sets the calculated inverse of the input matrix
    setinverse <- function(inverse) i <<- inverse
    
    ##gets the inverse (calculated or not, so can be NULL) of the input matrix
    getinverse <- function() i
    
    ##return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##returns a matrix that is the inverse of 'x', calculating it if it is not
##already cached
##inputs:
##x - the matrix cache (created by makeCacheMatrix) we want to calculate the 
##inverse of
##... - arguments passed to solve()
cacheSolve <- function(x, ...)
{
    ##first, check to see if we already have a cached inverse
    i <- x$getinverse()
    if(is.null(i))
    {
        ##no cached inverse, so calculate it
        data <- x$get()
        i <- solve(data, ...)
        
        ##cache it for later
        x$setinverse(i)
    }
    
    ##return the inverse
    i
}
