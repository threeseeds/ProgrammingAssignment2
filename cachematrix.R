## These two functions allow a user to create a matrix and solve its inverse 
##and cache that inverse, which is computationally expensive, for repeated use.

## This function creates a list of functions and the initial data values of x and m
## which will be available in the parent environment 

mx <- matrix (c(4,2,7,6), nrow=2, ncol=2)
makeCacheMatrix <- function(x = matrix()) {    #argument x will be preserved in the parent environment
  m <- NULL                                    #initializes data variable m
  set <- function(y) {                         # create setter for x and re-set m to null
    x <<- y                                    
    m <<- NULL
  }
  get <- function() x                          #create getter for x
  setmatrix <- function(matrix) m <<- matrix   #create setter for matrix and assign that value to m
  getmatrix <- function() m                    #create getter for m, which is the matrix
  list(set = set, get = get,                   #create the list of functions; this makes the functions and environment available in parent env
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

mcm <- makeCacheMatrix(mx)    #calls the makeCacheMatrix function to build the matrix and to have the object of type "makeCacheMatrix" for use in cachSolve

## This function finds the inverse of the matrix and caches it so that repeated calls use the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()                ## get the matrix from the object 'mcm'
  if(!is.null(m)) {                 ## this conditional returns the cached data if there is a value stored in the data object 'm'
    message("getting cached data")
    return(m)
  }
  data <- x$get()        #if 'm' is null then this line calls the get function in the list to get the matrix
  m <- solve(data, ...)  # then this line solves the matrix
  x$setmatrix(m)         # This calls the setmatrix function and sets the value to the solved matrix value
  m                      # this line returns the solved value
}

cacheSolve(mcm)
cacheSolve(mcm)

