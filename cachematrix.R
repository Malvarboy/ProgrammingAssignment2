## The functions computes the inverse of a matrix, caches (stores) inverse of the matrix in memory
## and pulls the inverse of the matrix from memory without redoing the computation

## makeCacheMatrix funtion creates a makeCacheMatrix object that is the input for cacheSolve
## function. The output is a list composed of the following: set, get, setinv_mat, get_inv_mat

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv_mat <- function(solve) inv_mat <<- solve
  getinv_mat <- function() inv_mat
  list(set = set, get = get,
       setinv_mat = setinv_mat,
       getinv_mat = getinv_mat)

}


## The cachSolve function returns the inverse of the matrix x. It first checks whether there is
## already an inverse of the the matrix stored in the memory, if none, it gets the matrix (x input
## in the makeCacheMatrix function, computes the inverse of x (using 'solve' function), stores 
## inverse in memory and returns the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv_mat <- x$getinv_mat()
          if(!is.null(inv_mat)) {
            message("getting cached data")
            return(inv_mat)
          }
          data <- x$get()
          inv_mat <- solve(data)
          x$setinv_mat(inv_mat)
          inv_mat
}

