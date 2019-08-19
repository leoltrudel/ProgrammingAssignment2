## makeCacheMatrix stores the value of four functions and stores the value of y in x, and of NULL in i, outside of the function environment into the outer environment. 
## makeCacheMatrix then stores set(), get(), set_inverse(), and get_inverse() as a list, and then outputs the list into a variable.  

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) i <<- inv
  get_inverse <- function() i
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve() is checking if get_inverse exists and if not, the function computes the value of the inverse with the solve() function. 
## If get_inverse() has already been solved, cacheSolve() returns the message "getting cached data" and returns the value of the inverse matrix. 

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
