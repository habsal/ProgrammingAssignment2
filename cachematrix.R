##makeCacheMatrix:
##This function creates a special "matrix" object that can cache its inverse.
##When the function called inverse matrix "inver" set to null  
##setinverse adds the inverse matrix to the cache 
##getinverse gets the inverse matrix from the cache




makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(y){
  x<<-y
  inver<<-NULL
}
get<-function() x
setinverse<-function(solve) inver<<- solve
getinverse<-function() inver
list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}


##cacheSolve:
##This function does: if matrix new then inverse matrix. If the matrix a old one already 
## calculated yhen bring the inverse matrix from the cahe

cacheSolve <- function(x=matrix(), ...) {
    inver<-x$getinverse()
    if(!is.null(inver)){
      message("getting cached data")
      return(inver)
    }
    datos<-x$get()
    inver<-solve(datos, ...)
    x$setinverse(inver)
    inver
}
