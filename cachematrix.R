## Put comments here that give an overall description of what your
## This function generates a matrix and calculates the inverse

## Function for generating the matrix

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


# Function for calculating the inverse of the previously generated matrix

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

