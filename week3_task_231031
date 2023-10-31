makeCacheMatrix <- function(mat0 = matrix()) {  
  inv <- NULL   
  set <- function(cat) { 
    mat0 <<- cat     
    inv <<- NULL   }   
    get <- function()
    mat0 
    setinverse <- function(inverse) 
    inv <<- inverse   
    getinverse <- function() inv  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}  

cacheinverse <- function(mat0, ...) {
  inv <- mat0$getinverse()
  if(!is.null(inv)) {
    message("get cache")
    return(inv)
  }
  matrix_to_invert <- cacheinverse <- function(mat0, ...) {
  inv <- mat0$getinverse()
  if(!is.null(inv)) {
    message("get cache")
    return(inv)
  }
  matrix_to_invert <- mat0$get()
  inv <- solve(matrix_to_invert, ...)
  mat0$setinverse(inv)
  inv
  }$get()
  inv <- solve(matrix_to_invert, ...)
  mat0$setinverse(inv)
  inv
}
