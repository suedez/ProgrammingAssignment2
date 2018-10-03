## 1)set a matrix(inversible only) as you like. 
## 2)assign the inverse of this matrix if you want, and both of
##them are gonna be cached. If you don't want to compute the 
##inverse,you can just skip this step.
## 3)the function"cacheSolve" will automaticalluy test if the set 
## matrix has already had a inverse.
## 4)if yes,it will return the cached result just as you 
## assigned before; if not, it will compute the inverse, cache it, 
## and show you the result.
 
## creates a"matrix" object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setMatrix<-function(solve) m<<-solve
  getMatrix<-function() m
  list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)
}


## This function checks the existence of inverse first for avoiding 
## needless computing,then makes sure there will be a inverse
## cached and returned.

cacheSolve <- function(x, ...) {
  m<-x$getMatrix()
  if(!is.null(m)){
    message("Here's cached data!")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setMatrix(m)
  m   ## Return a matrix that is the inverse of 'x'
}