## Calculate a inverse of a given matrix and save it in cache

## Is required to populate and/or retrieve the inverse from an object of type makeCacheMatrix().

makeCacheMatrix <- function(x = matrix()) { # x is initialized as a function argument
  i <- NULL #i is inverse of x, set to NULL initializing it as an object within the makeCacheMatrix() environment 
  set <- function(y) 
  {
    x <<- y #Assign the input argument to the x object in the parent environment
    i <<- NULL #Assign the input argument to the i object in the parent environment
  }
  get <- function() x #retrieving the value of x from the parent environment of makeCacheMatrix()
  setinverse <- function(inversa) i <<- inversa #assign the input argument to the value of i in the parent environment.
  getinverse <- function() i  #Again to retrieve the value of i from the parent environment of makeCacheMatrix()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  #assigns each of these functions as an element within a list(), and returns a fully formed object of type makeCacheMatrix() to the parent environment.

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #get the inverse from matrix x
  if(!is.null(i)) { #ask if there's a inverse calculated 
    message("getting cached data") # return  the inverse to the parent environment and a message
    return(i)
  }
  data <- x$get() #if no, get via argument input x
  i <- solve(data, ...) #Compute the inverse of x
  x$setinverse(i) #setting and save the value of inverse  
  i # and return it to the parent environment
}

#creating a makeCacheMatrix object 

x <- matrix(1:4, ncol = 2, nrow = 2)
makeCacheMatrix.object <- makeCacheMatrix(x)
cacheSolve(makeCacheMatrix.object)