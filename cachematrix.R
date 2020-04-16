## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a matrix and caches it's inverse
# Input : Matrix
# Output : Matrix with get/set inverse and values
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  # We have getter/setter for Matrix
  get<-function() x
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  getinv<-function() inv
  setinv<-function(inverse) inv<<-inverse
  
  list(get=get,set=set,getinv=getinv,setinv=setinv)
}


## Write a short comment describing this function
#Computes the inverse of matrix. It returns cached
# inverse if calculated before
# Input: Matrix
# Ouptut : Inverse of Matrix
cacheSolve <- function(x, ...) {
  
  inv<- x$getinv()
  
  if (!is.null(inv)){
    message("inverse in cached")
    return(inv)
  }
  
  m<-x$get()
  inv<-solve(m, ...)
  x$setinv(inv)
  
  return (inv)
        ## Return a matrix that is the inverse of 'x'
}
