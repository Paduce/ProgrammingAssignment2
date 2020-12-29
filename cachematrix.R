#makeCacheMatrix calculates the inverse of the matrix and stores
#it in a vector x, its stored in a different environnement from
#cacheSolve, wich acts as a sort of cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#cacheSolve searches in the global environnement for
#x and looks at the getinverse component to see if
#the inverse of this matrix was stored already, if not, it will calculate it an
#store it in the cache
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}




