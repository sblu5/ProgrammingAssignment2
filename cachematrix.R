## Put comments here that give an overall description of what your
## functions do

##Cache the inverse of a matrix

##TestCase: m1 <- makeCacheMatrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE))

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #1.  set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #2.  get the value of the matrix
  get <- function() x
  #3.  set the value of the inversion
  setsolve <- function(solve) m <<- solve
  #4.  get the value of the inversion
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}



# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
}

