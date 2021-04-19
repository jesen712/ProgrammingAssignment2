## makeCacheMatrix creates an R object that stores matrix and its inverse
## makeCacheMatrix contains 4 functions which are saved as list elements
## 1. set()
## 2. get()
## 3. setinverse()
## 4. getinverse()

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL 
  ## 'x' is an empty matrix
  ## initialised with default value to prevent errors in mat <- x$get()
  ## 'inver' stores to the inverse of matrix, initialised as NULL object
  
  ## set () assigns input argument y to 'x' in parent environment and
  ## assigns NULL value to inver object in parent environment, also 
  ## clears any value of inver cached by previous execution of cacheSolve()
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  ## get() defines the getter for matrix 'x'
  ## In this function 'x' is retrieved from parent environment and 
  ## not defined within get()
  get <- function() x
  
  ## setinverse() defines the setter for inverse 'inver'
  ## inver is defined in parent environment and needs to be accessed after
  ## setinverse completes, therefore uses "<<-" assignment operator
  setinverse <- function(inverse) inver <<- inverse
  
  ## getinverse() defines the getter for inverse 'inver' 
  getinverse <- function() inver
  
  ## Finally each of these functions are assigned as an element within a list()
  ## and returns it to the parent environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve function requires an argument that is returned by 
## makeCacheMatrix()to retrieve the inverse of a matrix from the 
## cached value that is stored in the makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  ## This part of the code calls getinverse() on the input object
  ## If a cached inverse matrix is available for the input object, it is returned
  ## and a message is displayed
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached matrix inverse data")
    return(inver)
  }
  
  ## Incase a NULL value is returned it means a cached inverse is not available
  ## New inverse matrix is calculated using solve() function on matrix 'mat' which 
  ## gets values from matrix 'x'
  ## This newly calculated inverse is then cached using setinverse() function
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setinverse(inver)
  inver
  
  ## Return a matrix that is the inverse of 'x'
}

