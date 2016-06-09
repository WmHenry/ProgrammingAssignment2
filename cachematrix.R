
    # syntax orgenized to be visually clean. 
    # two functions that cache for the inverse of a matrix.


makeCacheMatrix <- function(x = matrix())   {
  cachedInverse <- NULL
  setInv <- function(y)                       {
    
    # nested objects 'x', 'cashedInverse' in set environment.
    
    x <<- y
    cachedInverse <<- NULL
  }
  
  getnested <- function() x
  setInverse<- function(inverse) cachedInverse <<- inverse
  getInverse<- function() cachedInverse
  
  list(setInv = setInv, getnested = getnested,
       setInverse = setInverse,
       getInverse = getInverse)
}


# function for the inverse of an cacheMatrix object

cacheSolve <- function(x, ...)   {
  
  # matrix return that is the inverse of 'x'
  
  invFunc <- x$getInverse()
  if(!is.null(invFunc))        {
    message("getting cached data")
    
    # returning the 'invFunc' object
    
    return(invFunc)
  }
  data <- x$getnested()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
