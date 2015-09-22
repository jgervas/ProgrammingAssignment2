
 #########################################################################
  # Function makeCacheMatrix - Create Cacheable vector
  # Author: Jorge Gervasio Pereira
  # Date  : Sept, 22 th, 2015
  
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) mat <<- solve
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


 #########################################################################
  # Function cacheSolve - Perform Inverse of Cacheable Matrix, only if Matrix
  # not already processed
  # Author: Jorge Gervasio Pereira
  # Date  : Sept, 22th, 2015

cacheSolve <- function(x, ...) {
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
