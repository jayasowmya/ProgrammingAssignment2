## Creating a matrix and calculating its inverse matrix

makeCacheMatrix <- function(x = matrix()) { # function definition
  m<-NULL                                   # initializing m to be null vector as default
  setmatrix<-function(y){                   # setting a value for the matrix
    x<<-y                                   # caches the input matrix
    m<<-NULL                                # sets m to null to check whether cacheSolve was used
  }
  getmatrix<-function() x                   # retreives the matrix
  setinverse<-function(solve) m<<- solve    # function to calculate inverse of the matrix
  getinverse<-function() m                  # retrieves inverse matrix
  list(setmatrix=setmatrix,                 # creates a list containing the above functions
       getmatrix=getmatrix,
       setinverse=setinverse,getinverse=getinverse)
}

## Calculates Inverse for the above created matrix
cacheSolve <- function(x=matrix(), ...) {   # function definition
  m<-x$getinverse()                         # m gets the value of the inverse matrix if already calculated
  if(!is.null(m)){                          # checks whether the inverse matrix has already been computed or not    
    message("getting cached data")          # if yes then it retrieves the inverse from cache
    return(m)                               # exits from this function by returning value from cache
  }
  matrix<-x$getmatrix()                     # if no then its gets the new matrix 
  m<-solve(matrix, ...)                     # calculates inverse for the new matrix
  x$setinverse(m)                           # sets the above created inverse matrix for later retreivals
  m                                         # returns the new inverted matrix
}