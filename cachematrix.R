
## makeCacheMatrix creates a list containing  functions that sets or calls a matrix 
##and its inverse if the inverse has been calculated and cached.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##This step defaults the inverse to Null. This tells other functions
            ##the inverse has not been calculatd yet.
  
  ##This sets the value of the matrix to the value y. This can be used to redefine
  ##the cached matrix after the list has been made. When a new matrix is set in
  ##the list, the inverse matrix is set back to Null.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get Prints the value of the cached matrix.
  get <- function() x ##
  
  ## setinv This lets the user define the inverse of the matrix to whatever they want.
  setinv <- function(inverse) i <<- inverse 
  
  ##getinv Prints the inverse of the cached matrix. This value is Null if the
  ##inverse has not been calculated (via cacheSolve) or defined via setinv.
  getinv <- function() i
  
  ##this creates the list that includes functions to call or set the cached matrix
  ##and the functions to call or set its cached inverse.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve (input is the list created from makeCacheMatrixfirst calls 
##the list of the matrix to see if the inverse of the
##matrix has already been assigned in the list. If it has been then that 
##inverse wil be called. If it has not been assigned, then the function calculates
##the inverse and assigns it to the list cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()##This calls the inverse as defined in list of the cached matrix
  if(!is.null(i))##If the inverse has been defined, then is.null is false 
    {
    message("Getting cached inverse")
    return(i) ##Return prints the inverse and exits the function
  }
  
  ##If the inverse in getinv is still Null, this portion calculates the inverse.
  data <- x$get()##retrives the value of the matrix
  i <- solve(data, ...)##This calculates the inverse of the matrix
  x$setinv(i)##This step writes the just calculated inverse to the cached list
             ## created by makeCacheMatrix.
  i##This prints out the inverse matrix. 
}