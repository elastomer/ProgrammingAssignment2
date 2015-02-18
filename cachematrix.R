## Calculate Inverse of Matrix with Caching

## If Matrix exists, return the cached matrix from the parent environment
## If Matrix inverse does not exist, then calculate the Matrix Inverse using the solve function

## Two main functions makeCacheMatrix and cacheSolve
## Function makeCacheMatrix checks for cached matrix and passed to Parent environment
## Function cacheSolve returns Cached Matrix if exists or calculates inverse if Inverse is null

## Start of Function makeCacheMatrix - checks for cached matrix and passed to Parent environment

makeCacheMatrix <- function(x = matrix()) {

## Initialize variable m
  m <- NULL

## Function called set passes y and m to parent environment with the values assigned
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

## Function called get returns the called matrix
  get <- function() x
  
## Function called setinverse passes the variable called solve or the inverted matrix to
##   the variable m in the parent environment
  setinverse <- function(solve) m <<- solve

## Function called getinverse returns the variable m starting from the parent environment where
##   where m is the inverse
  getinverse <- function() m

## Create list for set, get, setinverse, getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
## End of makeCacheMatrix function  
}


## Start of Function cacheSolve - checks for existence of cached Matrix
##   If cached inverse matrix m exists, return it from the parent environment
##     else solve for the matrix inverse

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
## Check if the cached matrix m exists, if m is not null return m from parent enviroment
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

## Else the matrix inverse does not exist in m, and then solve for the inverse and pass to m
  data <- x$get()
  m <- solve(data, ...)

## Set the inverse matrix m to the parent enviroment
  x$setinverse(m)

## Return the inverse matrix m
  m

## End of cacheSolve function
}
