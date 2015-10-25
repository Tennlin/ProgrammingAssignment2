## Put comments here that give an overall description of what your
## functions do
## cachematrix function will get the inverse matrix when you input an invertible matrix
## the funciton use the lexical scope concept to complete. 

## Write a short comment describing this function
## makeCacheMatrix will produce the special object. 
## The special object can be used by the cacheSolve function
    

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  solv <- function(y) {       
        x <<- y            
        inverse <<- NULL        
  }                          
  get     <- function() x
  setsolv <- function(solve) inverse <<- solve
  getsolv <- function() inverse
  list(solv = solv, get = get,      
        setsolv = setsolv,
        getsolv = getsolv)
}


## Write a short comment describing this function
## cacheSolve function will produce the inverse matrix. it use the special object 
## and function that producing by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getsolv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolv(inverse)
  inverse
}
