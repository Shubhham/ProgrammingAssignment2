## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

 i <- NULL                  				## defining the inverse variable
 set <- function(y){					## define set with a new function
        x <<- y						## sets value in a different environment
	  i <<- NULL					## defining the inverse if matrix is new
	}
 get <- function()x					## defining the get function to return value of the matrix arg
 setinverse <- function(inverse) i <<- inverse  ## assigning inverse value
 getinverse <- function()i				## gets inverse value when called
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

      i <- x$getinverse()				 ## Return a matrix that is the inverse of 'x'
      if(!is.null(i)) {					 ## Check to see if i contains inverse matrix data
		message("getting cached data")	 ## display message
		return(i)					 ## return the stored value of i
	}
	data <- x$get()					 ## Calling the matrix and storing it in data
	i <- solve(data,...)				 ## Inverse the data and storing it in variable i
	x$setinverse(i)					 ## calling setinverse function
	i
}
