## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a matrix and
## a list of functions to
## set the value of the matrix
## get the value of the matrix
## store the inverse of a matrix in the cache
## get the value of the inverted matrix stored in the cache

makeCacheMatrix <- function(x = matrix()) {

     
     ## m is an object for storing the inverse of a matrix
     ## set m to NULL when makeCacheMatrix is run
     ## otherwise m may contain erroneous values from previous runs of this or other functions
     m <- NULL
     
     ## this function substitutes the matrix x with the matrix y in the main function (makeCacheMatrix)
     set <- function(y) {
          
          ## replace x with y in the main function environment
          x <<- y
          
          ## now that the values in x have changed, the inverse of x needs to be calculated anew
          ## set the cached inverse of the matrix x to null
          m <<- NULL
          
     } ## end set function
     
     ## this function returns the matrix x
     get <- function() x
     
     ## this function stores the "inverse" of the matrix x in m
     setInverse <- function(inverse) m <<- inverse
     
     ## this function returns the inverted matrix stored in m
     getInverse <- function() m
     
     ## makeCacheMatrix returns a list of functions. defined above:
     ## set(), get(), setInverse(), getInverse()
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
     
     ## for example, run makeCacheMatrix(x) like so
     ## a <-makeCacheMatrix(matrix(c(0,2,1,3), nrow=2, ncol=2, byrow = TRUE))
     
     ## then typing
     ## a$get() 
     ## returns the matrix x
     
     ## a$set(y) 
     ## replaces the matrix x with the matrix y in the main function environment
     
     ## a$setInverse(matrix(c(2,1,3,0), nrow=2, ncol=2, byrow= TRUE)) 
     ## stores a user defined matrix in m
     ## note: this is not necessarily the inverse of matrix x, just whatever is passed to the function
     
     ## a$getInverse() 
     ## returns the inverted matrix stored in m
     
     
} ## end makeCacheMatrix function




## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverted matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
  
     ## first get the values stored in m
     m <- x$getInverse()
  
     ## next check if there is actually something stored in m
     if(!is.null(m)) {
    
          ## if m does contain values, then we will use the cached data
          ## inform the user we are using cached data
          message("getting cached data")
    
          ## return the values stored in m, presumably an inverted matrix
          return(m)
     } ## end check for null values in m
  
     ## if m contains no values (is NULL) then we will need to calculate the inverse of the matrix x
     ## inform the user we must calculate the inverse of the matrix x
     message("calculating inverse of the matrix")
  
     ## first get matrix x
     matrix <- x$get()
  
     ## then calculate the inverse of matrix x and store the inverse in m
     m <- solve(matrix, ...)
  
     ## use the setInverse() function to cache the inverted matrix stored in m 
     ## This replaces the value m in the main environment of the makeCacheMatrix function with the
     ## inverted matrix calculated in this function and assigned to m locally. 
  
     x$setInverse(m)
  
     ## return the inverted matrix calculated above
     m
     
} ## end cacheSolve function