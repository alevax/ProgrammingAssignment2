##
# Computaiton of the inverse of a matrix
# by using chaching features
# --------------------------------------

## It creates a special matrix for the caching puprose
makeCacheMatrix <- function( .matrix = matrix() ) 
{
  .inverse <- NULL
  set <- function(myMatrix) {
    .matrix <<- myMatrix
    .inverse <<- NULL
  }
  get <- function() .matrix
  
  setInverse <- function(myInverse) .inverse <<- myInverse
  getInverse <- function() .inverse
  
  res <- list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
  return(res)
}

## It returns the inverse of the matrix
# If the inverse of the matrix is already computed and stored in the cache it is returning that one, 
# otherwise is computing the inversion ex novo
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  return(inverse)
}

##
# Running example
# ---------------
  set.seed(1986) # set seed for reproductibility
  nrows = 1000 # number of rows (and columns)
  mat <- matrix( runif(nrows^2) , ncol=nrows , nrow=nrows ) # fill a matrix with random data

  mat.cached <- makeCacheMatrix(mat) # create the matrix
  y <- cacheSolve(mat.cached) # solve for the inverse
  head(y) # check the result

  