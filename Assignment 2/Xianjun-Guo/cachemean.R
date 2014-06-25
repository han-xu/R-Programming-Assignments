cacheSolve <- function(x,...){
  matrix <- x$getsolve()
  if(!is.null(matrix)){
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data,...)
  x$setsolve(matrix)
  matrix
}