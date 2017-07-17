<<<<<<< HEAD
# makeCacheMatrix makes an object that allows for ## caching of a matrix and its inverse. cacheSolve
# returns the inverse stored in memory or calculates
# a new one if no inverse has been set yet. 


##Takes an nxn matrix 'mat' and returns a list of
##functions that allow setting and getting of said
##matrix/its inverse. Default value of input matrix is 
## an empty matrix. 
makeCacheMatrix <- function(mat = matrix()) {
  i <- NULL ##will store the inverse. 
  
  set <- function(y){ ##sets the matrix
    mat <<- y
    i <<- NULL
  }
  
  get <- function() mat ##gets the matrix
  
  setInv <- function(inv) { ##sets the inverse
    i <<- inv
  }
  getInv <- function() i ##returns the current value of the inverse
  
  ##returns a list of the above variables with named arguments
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
}


## Returns the determinant of a "makeCacheSolve" matrix by
## either calculating and setting it or returning it from memory. 
cacheSolve <- function(mat, ...) {
  inv <- mat$getInv()
  
  if(!is.null(inv)){
    return(inv)
  }
  newInv <- solve(mat$get())##calculate/store the inverse
  mat$setInv(newInv)
  newInv
}


=======
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
>>>>>>> 7f657dd22ac20d22698c53b23f0057e1a12c09b7
