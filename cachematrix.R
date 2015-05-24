## Compute, store and recall inverse of matrix from cache ##


## Create a special matrix object that stores and recalls matrix and its inverse from cache 

makeCacheMatrix <- function(x = matrix()) {
    
    ## set the value of matrix
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL 
    } 
    
    ## get the value of matrix 
    get <- function() x
    
    ## set the value of inverse of matrix (store the inverse matrix in cache)
    setinverse <- function(solve) s <<- solve
    
    ## get the value of inverse of matrix (recall the inverse matrix from cache)
    getinverse <- function() s
    
    ## create a list of above functions
    list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
  
  
## Caculate the inverse of special matrix object created with the above function, 
## after first checking in cache for existence of such matrix inverse (If present in cache, 
## recall the value for inverse, if not compute the inverse and store it in cache).
 
cacheSolve <- function(x, ...) {
     
     ## inquire particular matrix's cache for inverse value.
     s <- x$getinverse()
     
     ## if inverse of particular matrix exists in cache, recall its value. 
     if(!is.null(s)) {
         message("getting cached data")
         return(s)
     }
     
     ## if inverse of matrix is not found, continue computation. 
     ## Get the matrix from special matrix object.
     data <- x$get()
     
     ## calculate inverse matrix
     s <- solve(data, ...)
     
     ## set/store the inverse matrix in cache
     x$setinverse(s)
     s
}

