## Create two functions to allow caching potentially time-consuming/costly computation of matrix inversion.

## makeCacheMatrix creates special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
          m<-NULL
          set<-function(y) {
                x<<-y
                m<<-NULL
          }
          get<-function() x
          setinverse<-function(solve) m <<- inverse
          getinverse<-function() m
          list(set=set,get=get
               setinverse=setinverse
               getinverse=getinverse)
}


## cacheSolve computes inverse of special "matrix" returned by makeCacheMatrix. 
## If inverse already calculated (with unchanged matrix), cacheSolve retrieves inverse from the cache.


cacheSolve <- function(x, ...) {
          m<-x$getinverse()
          if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
          }
          data<-x$get()
          m<-solve(data,...)
          x$setinverse(m)       ## Return a matrix that is the inverse of 'x'
}
