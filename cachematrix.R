## Caching the Inverse of a matrix
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

Data_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) ##creating a matrix of 2 rows 2 columns
Data_Matrix$get() ## view the matrix

Data_Matrix$getinverse() #returns null as originally initialized

cacheSolve(Data_Matrix) ## apply the solve function
cacheSolve(my_matrix) ## getting cached data

Data_Matrix$getinverse()## display the cached inverse matrix

Data_Matrix$set(matrix(c(4, 3, 2, 1), 2, 2)) ## create a new matrix with new values and assign to Data_Matrix

Data_Matrix$get() ## view the matrix

Data_Matrix$getinverse() #returns null as originally initialized

cacheSolve(Data_Matrix) ## apply the solve function
cacheSolve(my_matrix) ## getting cached data

Data_Matrix$getinverse()## display the cached inverse matrix
