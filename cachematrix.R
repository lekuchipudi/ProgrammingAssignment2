## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCatcheMatrix creates a special "matrix", which is really 
#a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { #x initialization as function argument
  m <- NULL #m initialization to NULL;The object m will hold the inverse of the matrix that makeCacheMatrix was called with
  set <- function(y) {#argument can be of any object name other than x
    x <<- y #assigning input argument to x in parent environment 
    m <<- NULL #assigning m to NULL in parent environment and this clears any value of m that had been cached by a prior execution of catcheSolve
  }
  get <- function() x #defines getter for matrix x, x value is retrieved from parent environment of makeCatcheMatrix
  setsolve <- function(solve) m <<- solve #defines the setter for the inverse 
  getsolve <- function() m #R finds the correct symbol m to retrieve its value
  list(set = set, get = get,#above functions are assigned as an element within a list()
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
#The following function calculates the inverse of the special "matrix" created 
#with the above function. However, it first checks to see if the inverse has already 
#been calculated. If so, it gets the inverse from the catche and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the catche
#via the setsolve function.

cacheSolve <- function(x, ...) {#starts with a single argument x and an ellipsis that allows the caller to pass additional arguments into the function
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() #function attempts to retrieve a inverse from the object passed in as the argument
  if(!is.null(m)) {#checks whether above result is NULL, if not equal to NULL, we have a valid catched inverse and can return it to the parent environment
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m) #returns the value of inverse
  m
}
