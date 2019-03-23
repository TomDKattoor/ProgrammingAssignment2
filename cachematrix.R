# As matrix inversion is a costly function, following solution
# caches the inverse rather than computing it repeatedly.

## Function used to create a list, that has functions for
## getting and setting value of matrix
## getting and setting value of inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse) {
    cache <<- inverse
  }
  getinverse <- function()
    cache
  list (
    set = set,
    get = get,
    setinverse = setinverse ,
    getinverse = getinverse
  )
}


## Function returns inverse of a matrix
## It checks if inverse is already cached or not
## If not cached, it computes the inverse, caches it and then returns the value

cacheSolve <- function(x, ...) {
  inverse_mat <- x$getinverse()
  if (!is.null(inverse_mat)) {
    print("getting cached data")
    return(inverse_mat)
  }
  data <-  x$get()
  inverse_mat <- solve(data)
  x$setinverse(inverse_mat)
  inverse_mat
}



# testing script
# 2nd cacheSolve call should print "getting cached data"
#x = rbind(c(1, 3), c(4, 5))
#m = makeCacheMatrix(x)
#x
#m$get()
#cacheSolve(m)
#cacheSolve(m)
