## Coursera Assignment2, 
#  This function generates a matrix, from which the inverse is calculated

## Generates the Matrix

makeCacheMatrix <- function(x = matrix()) {
 i<-NULL
 set<-function(y)
  {
    x<<-y
    i<<-NULL
  }
 get<-function()x
 setinverse<- function(solve) i <<-solve
 getinverse<- function() i
 list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates de inverse

cacheSolve <- function(x, ...) {
   i<-x$getinverse()
   if(!is.null(i))
   {
     print("getting cached data")
     return(i)
   }
   data<-x$get()
   i<-solve(data, ...)
   x$setinverse(i)
   i
}
