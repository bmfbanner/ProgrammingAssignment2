## The functions below will create a special "matrix" object that can cache its invers and 
## then either calculate the inverse of the matrix or if the inverse has already been calculated 
## and the matrix has not been changed then it will simply return the cached inverse of the matrix 
## instead of run the calculations over again.


## This function creates a list containing a function to do the following:
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the Inverse of the Matrix
## Get the value of the Inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y) {
      x<<-y
      m<<-NULL
  }
  get<-function()x
  setmatrix<-function(solve) m<<-solve
  getmatrix<-function()m
  list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## This function calculates the inverse of the matrix created with the above function.
## It first checks to see if the inverse has already been calculated.
##If so it gets the mean from the cache and skips the computation
cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix,...)
    x$setmatrix(m)
    m
  ## Return a matrix that is the inverse of 'x'
}
