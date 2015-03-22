## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The assignment is to write a pair of functions that 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(data_matrix = matrix()) 
{
    
    stored_inverse   <- NULL
    
    set <- function(y) 
    {
      data_matrix <<- y
      stored_inverse <<- NULL
    }
    
    get <- function() 
    {
      return (data_matrix)
    }
    
    setinverse <- function(sent_replacement_inverse)
    {
      stored_inverse <<- sent_replacement_inverse
    }
    
    getinverse <- function() 
    {
      return(stored_inverse)
    }
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(madeMatrix, ...) 
{
        ## Return a matrix that is the inverse of 'x'
      
    local_inverse <- madeMatrix$getinverse()
    
    if(!is.null(local_inverse)) 
    {
      message("getting cached data")
      return(local_inverse)
    }
    else 
    {
      local_data <- madeMatrix$get()
      local_inverse <- solve(local_data, ...)
      
      madeMatrix$setinverse(local_inverse)
      return(local_inverse) 
    }
}
