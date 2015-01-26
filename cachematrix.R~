#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  getevn<- function() environment()
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve,
       getevn = getevn)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}