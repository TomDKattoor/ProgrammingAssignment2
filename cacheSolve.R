# function to create cache object
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

# function to calculate inverse using cache object
cacheSolve <- function(x) {
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
x = rbind(c(1, 3), c(4, 5))
m = makeCacheMatrix(x)
x
m$get()
cacheSolve(m)
cacheSolve(m)
