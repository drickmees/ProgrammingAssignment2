# Here my comments:
## Matrix inversion is a costly computation and it is recommend
## to caching the inverse of a it 

# makeCacheMatrix creates a list containing a function that
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x
  setsolve<-function(solveM) s<<- solveM
  getsolve<-function() s
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Next function returns the inverse of the matrix. First, it checks whether
## the inverse has already been computed, if it is true, then  it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## solve function. Of course thath this function assumes that the matrix has solution

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s<-x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  matrixsolve<-x$get()
  s<-solve(matrixsolve, ...)
  x$setsolve(s)
  s
}
