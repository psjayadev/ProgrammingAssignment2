## The following functions are used to calculate the inverse of a matrix and cache it
##  and to retrive it when required

## Function to set & get a matrix and also to set and get the value
## of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <-function(y){
    x<<-y
    i<<-NULL
  }
  get <-function() x
  setinverse <-function(inverse) i<<-inverse
  getinverse <-function () i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Checks if the inverse of x is available in cache
## and returns it, if not calculates inverse and returns it

cacheSolve <- function(x, ...) {
  i <-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i <-solve(data)
  x$setinverse(i)
  i
}
