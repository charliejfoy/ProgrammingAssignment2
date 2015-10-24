## These 2 functions serve as the wrapper functions to create and modify our 
## 'special' matrix.


## The makeCacheMatrix serves as the constructor and returns a list of pointers
## to the 4 child functions 

makeCacheMatrix <- function(x = matrix()) {
     ## instantiate m to NULL
     m <- NULL
     
     ## create a set function to set the matrix variable x using the super
     ## assignment to search through the parent environments before searching
     ## the global environment.  When we choose to set the matrix to a new value
     ## we also reset the calculated inverse to NULL, since it hasn't been 
     ## determined yet.
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     ## get function simply returns x (the matrix)
     get <- function() x
     
     ## setInverse function uses the super assignment to assign solve to m. 
     ## Could be dangerous if called directly, as the resultant inverse might
     ## not be reflective of our original matrix x.
     setInverse <- function(solve) m <<- solve
     
     ## getInverse function simply returns the Inverse matrix of x.
     getInverse <- function() m
     
     ## returns a list of pointers to functions.
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse) 

}


## cacheSolve function takes a matrix x and returns the inverse of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     ## assign the Inverse of x to m.
     m <- x$getInverse()
     
     ## if m to populated, and not NULL, then return the cached inverse, and end
     ## the function call
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## otherwise, get the original matrix x.
     data <- x$get()
     
     ## and calculate its inverse matrix using the solve function
     m <- solve(data, ...)
     
     ## then call the setInverse function with our calculated inverse to
     ## maintain our cached inverse data
     x$setInverse(m)
     
     ##display m
     m
}
