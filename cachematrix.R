# My comments:
## Matrix inversion is a costly computation and it is recommend
## to cach the inverse of a matrix 

# makeCacheMatrix creates a list containing a function that
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Next function returns the inverse of the matrix. First, it checks whether
## the inverse has already been computed, if it is true, then  it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## solve function. Of course thath this function assumes that the matrix has solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached values")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
