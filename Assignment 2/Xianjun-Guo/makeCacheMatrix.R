makeCacheMatrix <- function(x=matrix()){
  matrix <- NULL
  set <- function(y){
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setsolve <- function(m) matrix <<- m
  getsolve <- function() matrix
  list(set=set, get=get, 
       setsolve=setsolve, 
       getsolve=getsolve)
}