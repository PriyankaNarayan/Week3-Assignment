# This function creates a special matrix

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(inverse) m <<- inverse
  getm <- function() m
  list(set = set, get = get,
       setm = setm,
       getm = getm)
}	

#This function computes the inverse of the matrix and caches it

cacheSolve <- function(x, ...){
  m <- x$getm() 
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data <- x$get()	
  mat <- solve(data)
  x$setm(mat)
  mat
}


#This lines will help us compute the inverse of a matrix

mymat <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(mymat)
