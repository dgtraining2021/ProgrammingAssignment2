
# Goal: Retuan a list of functions about X 
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 # reassign value of x to y  
 set <- function(y){
   x <<- y
   m <<- NULL
 }
 # get x
 get <- function() x
 # set inverse of x 
 setinverse <- function(inverse) m <<- inverse
 # get inverse of x
 getinverse <- function() m
 return(list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse))
}



## Perform calculation of getinverse

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  return(m)
}







