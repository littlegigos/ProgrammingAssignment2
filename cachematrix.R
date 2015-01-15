 makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
     x <<- y
     m <<- NULL
   }
   get <- function () x
   setrev <- function(solve) m <<- solve
   getrev <- function() m
   # get list
   list(set = set, get = get, setrev = setrev, getrev = getrev)
 }
 
 
 
 cacheSolve <- function(x, ...) {
   m <- x$getrev()
   if(!is.null(m)){
     message("getting cached data")
     return(m)        
   }
   data <- x$get()
   m <- solve(data,...)
   x$setrev(m)
   m
 }
