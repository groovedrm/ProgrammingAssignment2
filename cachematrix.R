# First function to variable assignment, get, set, etc.
makeCacheMatrix <- function(x = matrix()) {
  inv_mtx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mtx <<- inverse
  getinverse <- function() inv_mtx
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse
  )
}

# For inverting, looking in cache, retrieving from cache
cacheSolve <- function(x, ...) {
  inv_mtx <- x$getinverse()
  if(!is.null(inv_mtx)) {
    message("Loading from cache")
    return(inv_mtx)
  }
  data <- x$get()
  inv_mtx <- solve(data)
  x$setinverse(inv_mtx)
  inv_mtx
}