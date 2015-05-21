##Programming Assignment 2, R Programming on Coursera

##Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly.

##These two functions can be used to calculate and cache the inverse of a 
##matrix.

##The first function creates a special matrix object, it contains several functions
##which set the value of the matrix, get the value of the matrix, 
##set the inverse and get the inverse. The second function checks if the
##inverse has already been calculated before calculating it (using solve()) 
##and caching it in the special matrix object. If the inverse had already 
##been calculated it will return the cached inverse instead.

##In comments "matrix" refers to original matrix and "inverse" refers to the
##inverse of the original matrix

##The makeCacheMatrix function takes a matrix as an argument (error will
##be returned if argument is wrong class). A special matrix object is 
##returned and within this object several functions are defined and the
##inverse is set to NULL; this ensures the inverse is set to NULL if the
##matrix is changed so that it will be recalculated by the second function.

makeCacheMatrix <- function(x = matrix()) {
  #set inverse to NULL
  i <- NULL
  #set function to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get function to get the matrix
  get <- function() x
  #setinverse function - doesn't do any calculations, manually sets inverse
  setinverse <- function(inverse) i <<- inverse
  #getinverse function - returns inverse
  getinverse <- function() i
  
  #places all of the above functions into a list.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks if the inverse has already been calculated,
##if it has not, it is calculated, cached and returned. If it has been
##calculated before, the cached inverse is returned instead.


#x is the object returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
  #set inverse (i) by calling getinverse function defined within 
  #makeCacheMatrix (if it has already been calculated this will retrieve it)       
  i<-x$getinverse()
  #check if there is a value in i, if so, print a message and return 
  #that value
  if(!is.null(i)) {
    message("Getting cached matrix")
    return(i)
  }
  #if there wasn't a value in i, retrieve the matrix
  data<-x$get()
  #and calculate the inverse
  i<-solve(data,...)
  
  #call setinverse function from makeCacheMatrix to cache the inverse
  #for future use
  x$setinverse(i)
  
  #return the inverse
  return(i)
}
