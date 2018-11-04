## This is a couple of functions which aims to cache and compute matrix inverse
## This function exist to create a special kind of matrix objects

makeCacheMatrix <- function(x = matrix()) {
inver <- NULL
set <- function(a) 
{
  x <<- a;
  inver <<- NULL;
}
get <- function() return(x);
setinver <- function(inver) inver <<- inverse;
getinver <- function() return(inver);
return (list(set = set, get = get, setinver = setinver, getinver = getinver))
}


## In this function inverse of matrix from "makeCahceMatrix" should be computed. Also it looking if matrix was already calculated,
## In this case "cacheSolve" retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver))
  {
    message("Getting catched Data")
    return(inver)
  }
  data <-x$get()
  inver <- solve(data,...)
  x$setinver(inver)
  inver
  
}
