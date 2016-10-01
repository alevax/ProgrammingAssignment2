##
# Computaiton of the inverse of a matrix
# by using chaching features in R
# --------------------------------------

## 
# makeCacheMatrix: function
#   It creates a special matrix for the caching purpose
# -----------------------------------------------------
makeCacheMatrix <- function( .matrix = matrix() ) 
{
  .inverse <- NULL
  set <- function(myMatrix) {
    .matrix <<- myMatrix
    .inverse <<- NULL # the inversion is not existing yet because it is a new matrix object
  }
  get <- function() .matrix # return the matrix
  
  setInverse <- function(myInverse) .inverse <<- myInverse
  getInverse <- function() .inverse
  
  res <- list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
  return(res)
}

## 
# cacheSolve: function
# It comnpute and returns the inverse of the matrix
#   If the inverse of the matrix is already computed and stored in the cache it is returning that one, 
#   otherwise is computing the inversion ex novo
# ----------------------------------------------------------------------------------------------------
cacheSolve <- function(x) 
{
  inverse <- x$getInverse() # Get the inverse (cached)
  
  if(!is.null(inverse)) { # test if the inverse is already computed
    message("getting cached data")
    return(inverse) #return in that case 
  }
  # It is continuing if the inverse object (the cached one) is empty
  data <- x$get() # get the matrix
  inverse <- solve(data) # compute the inversion
  x$setInverse(inverse) # save in the cache
  return(inverse)# return the inverted matrix
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

  