## Function makeCacheMatrix is a function that creates a matrix object which can cache 
## its inverse.
## Function cacheSolve is a function that creates a matrix inverse for a matrix created with 
## a makeCacheMatrix function.

## makeCasheMatrix is actually a list of functions
## setMatrix sets the value of the matrix
## getMatrix gets the matrix
## setinv sets matrix inverse
## getinv gets matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  setMatrix<-function(y) {
    x<<-y
    inv<<-matrix(,nrow(y),ncol(y))
  }
  getMatrix<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinv = setinv, getinv = getinv)

}


## Funtion calculates the inverse of the matrix. If the inverse is already calculated function
## returns value from the cache.

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        l<-sum(as.vector(is.na(inv)))
        if(l<1) {
          message("getting cached data")
          return(inv)
        }
        M<-x$getMatrix()
        inv<-solve(M)
        x$setinv(inv)
        inv      
}
