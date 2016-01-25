#most of the code is borrowed from the example provided with changes to the variable and function names. 
#makecacheMatrix stores four functions -> get(),set(),setinv(), and getinv()
#list can be used to store function!!(Had trouble figuring out this part)
#set function is used for defaulting and setting up the value of the input(Y is a global vairable, and x is recycled for every run)
#get returns the matrix stored in the main function.It does not need any argument.
#SetInv is used for storing the inverse of the input matrix(passing Y to set mean, Y is stored with value of X in set())
#GetInv returns the value set in getinv

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function()
  {
    return	x
  }
  
  setinv <- function(inverse){
    dolve(inverse)
    inv <<- inverse
  }
  getinv <- function() {
    return inv
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function inverses the matrix input in the previous function.Solve() is used for inversing the matrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")     
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
