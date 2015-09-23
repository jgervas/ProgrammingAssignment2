 ############################################################################################
 ## Function makeCacheMatrix -  Uses the factory design pattern to create a object with an 
 ##                             a cacheable Matrix  as data and the methods to operate over it  
 ##                             - get, getinv, set, setinv
 ##
 ## Steps to use:     1) Create object using makeCacheMatrix: MatQ <- makeCacheMatrix( matrixM); matrixM must be
 ##                        square inversable matrix
 ##
 ##                   2) Submit MatQ to cacheSolve          : cacheSolv(MatQ); in case of MatQ has been already processed
 ##                                                           the message "getting cached data" will be displayed
 ## Author: Jorge Gervasio Pereira
 ## Date  : Sept, 23th, 2015

  makeCacheMatrix <- function (x = matrix()) {
  
 # First things first: test vect to verify if  matrix was passe as argument
    if (!is.matrix(x)) {
      stop("Input vector must be a square inversable  matrix !")
    } 
 # Clean scrap matrix user to store values
    mat <- NULL
    set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  # Functions/method to operate over cacheable matrix data
    get    <- function() x                  # retur cacheable matrix
    setinv <- function(solve) mat <<- solve # defines the inverse of the matrix using  R function solve
    getinv <- function() mat                # returns the inverse of the matrix
 
  # defines the return list with the methods associated to the object
    list(set = set, get = get,setinv = setinv, getinv = getinv)
  
  
}
#######################################################################################
## Function cacheSolve - Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse
## Author: Jorge Gervasio Pereira
## Date  : Sept, 23th, 2015

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mat <- x$getinv()
  
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  data <- x$get()
  mat <- solve(data)
  
  x$setinv(mat)
  mat
  
  
}
