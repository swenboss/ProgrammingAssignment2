## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseVar <- NULL
  setVar <- function(y) {  ## set the value of the matrix
    x <<- y
    inverseVar <<- NULL
  }
  getVar <- function() x   ## get the value of the matrix
  SetInverseVar <- function(inverse) inverseVar <<- inverse  ##set the value of inverse of the matrix
  GetInverseVar <- function() inverseVar ##get the value of inverse of the matrix
  list(setVar=setVar,
       getVar=getVar,
       SetInverseVar=SetInverseVar,
       GetInverseVar=GetInverseVar)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverseVar <- x$GetInverseVar()
  if(!is.null(inverseVar)) { ##check if the inverse data are alreadi in cache
    message("Cached data are being loaded.") ##when yes, they are loaded
    return(inverseVar)
  }
  mydata <- x$getVar() ##if they are not in cache, they'll be computed 
  inverseVar <- solve(mydata)
  x$SetInverseVar(inverseVar)
  inverseVar
}
