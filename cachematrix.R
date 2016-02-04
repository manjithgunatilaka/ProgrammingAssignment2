## There are two functions, makeCacheMatrix() creates a matrix and 
## and cache its contents. The other function, cacheSove()
## creates an inverse matrix for a given matrix. If the inverse matrix
## is already not in cached it creates a matrix and cached it. Otherwise
## it will retunr cached inverse matrix

## # ---------------------------------------------------------------
## This function creates a matrix and cached it. It has four sub functions
## set(), get() , getInv() , setInv()
## ---------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  
  invMtx <- NULL
  
  set <- function(y) {
    x <<- y
    invMt <<- NULL
    
  }
  get <- function() x
  getInv <- function() invMtx
  setInv <- function(inv) invMtx <<- inv
  
  list(set = set, get = get, setInv = setInv , getInv = getInv)
  
}


## Creates an inverse matrix if matrix is not chached, otherwise
## it will return cached inverse matrix.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mtx <- x$get()
  inv <- solve(mtx)
  
  if (class(try(solve(mtx),silent=TRUE))=="matrix") {
    inv <- solve(mtx)
  }
  else
  {
    message("cannot find inverse of the matrix, determinant is zero")
    inv <- NULL
  }   
  x$setInv(inv)
  inv
}