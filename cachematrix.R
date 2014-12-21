#this function takes a matrix object to creates a special 'matrix'.
makeCacheMatrix <- function(x = matrix()) {
  #'ix' represents the inverse of matrix; assign NULL to it initially
  ix <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  #the next three functions are to be called in the 'cacheSolve' function
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  #the function returns a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#this function takes the special matrix created by 'makeCacheMatirx' function, and gets the inverse of the matrix.
cacheSolve <- function(x, ...) {
  #run the getinverse() function
  ix <- x$getinverse()
  #if ix is not null, it means the inverse is already calculated and cached
  #then we can just use the cached value
  if(!is.null(ix)) {
    message("getting cached inverse matrix")
    return(ix)
  }
  #else, if ix is null, need to calculate the inverse of the matrix
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}

