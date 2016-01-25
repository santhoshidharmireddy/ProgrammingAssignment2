################################################################################################################
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#Below are two functions that are used to create pair of functions that cache the inverse of a matrix.
################################################################################################################

#The first function, makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
#The second function, cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

#set the value of the matrix
#get the value of the matrix
#set the value of inverse of the matrix
#get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The following function calculates the inverse of the square matrix created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the 
#setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}




###Sample Run
x=rbind(c(1, -1/4), c(-1/4, 1))
z<- makeCacheMatrix(x)
z$get()
cacheSolve(z)

cacheSolve(z)