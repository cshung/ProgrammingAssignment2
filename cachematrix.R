#
# Create an instance of a cacheMatrix
#
makeCacheMatrix <- function(underlyingMatrixInstance) {

  #
  # Initialize the cache to be NULL
  #
  cachedMatrixInverse <- NULL

  #
  # Retrieving the underlying matrix instance
  #
  get <- function()
  {
    underlyingMatrixInstance
  }

  #
  # Changing the underlying matrix instance
  #
  set <- function(anotherMatrixInstance)
  {
    underlyingMatrixInstance <<- anotherMatrixInstance

    # Note that updating the underlyingMatrix instance invalidates the cache
    cachedMatrixInverse <<- NULL
  }

  #
  # Retrieving the cached matrix inverse - note this could return NULL
  #
  getCachedMatrixInverse <- function()
  {
    cachedMatrixInverse
  }

  #
  # Changing the cached matrix instance - intended for cacheSolve to use only
  #
  setCachedMatrixInverse <- function(Solve)
  {
    cachedMatrixInverse <<- Solve
  }

  # Bundle all the elements and return as a list (consider this list an representation for the object)
  list(get = get, set = set, getCachedMatrixInverse = getCachedMatrixInverse, setCachedMatrixInverse = setCachedMatrixInverse)
}

#
# Compute the inverse of a matrix
#
cacheSolve <- function(cacheMatrix, ...)
{
  matrixInverse <- cacheMatrix$getCachedMatrixInverse()
  if(!is.null(matrixInverse))
  {
    # If we have a cached matrix inverse, return it
    message("getting cached data")
    return(matrixInverse)
  }

  # The cache is not available, compute the inverse the hard way
  data <- cacheMatrix$get()
  matrixInverse <- solve(data, ...)

  # Cache the hard work
  cacheMatrix$setCachedMatrixInverse(matrixInverse)

  matrixInverse
}