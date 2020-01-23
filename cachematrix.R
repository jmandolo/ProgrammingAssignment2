#creating a matrix object which will be able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initiating the inverse
    
    ##setting the matrix
  set <- function(y) {
          x <<- y
          i <<- NULL 
  }
  ##getting the matrix
  get <- function() x
  #setting inverse
  setinverse <- function(inverse) 
    i <<- inverse
  #getting inverse
  getinverse <- function() 
    #returning inverse
    i
  #Returning list of methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##compute the inverse of matrix above which was returned by "makeCacheMatric". If inverse has already been calculated, 
##cachesolve will retrieve the inverse from cache
cacheSolve <- function(x, ...) {
  
  #return a matrix that is inverse of 'x'
  i <- x$getinverse()
  
  #return inverse if it has already been set
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  
  #getting matrix from object
  data <- x$get()
  
  #calculating inverse
  i <- solve(data, ...)
  
  #solve inverse and return matrix
  x$setinverse(i)
  i
}

##test
x <- matrix(c(4,6,7,8,9,10,3,3,4),3,3)
y <- makeCacheMatrix(x)
y$get() #getting the matrix

cacheSolve(y) #computing inverse

cacheSolve(y)#getting from cached data
