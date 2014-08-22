## The code below is for caching the inverse of a matrix. 
##This pair of functions are used to create a special object that stores a matrix and chaches its inverse
## <<- operator is used to assign a value to an object in a different environment from the current one.

## First function is used to create a "special" matrix wich is a list containing a function to:
## set and get the value of the matrix
## set and get the inverse of the matrix

makeMatrix <- function(x = matrix()) {
      inverse_matrix <- NULL
      set <- function(y) {                                   #sets the values of a new matrix and clean the value of inverse_matrix
          x <<- y
          inverse_matrix <<- NULL
      }
      get <- function() x                                    #gets the values of the current matrix 
      setinverse <- function(solve) inverse_matrix <<- solve #calls the cacheInverse function to calculate or return the cache value (if it has already been calculated) of the inverse of the matrix
      getinverse <- function() inverse_matrix                #gets the value of inverse_matrix of the cacheInverse function
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function calculates the inverse of the matrix defined with the makeMatrix funtion:
## First step is checking if the inverse has already been calculated. If so, the function gets the inverse from the cache
## Otherwise, the function has to calculate the inverse of the matrix by means of the solve() function

cacheInverse <- function(x, ...) {
      inverse_matrix <- x$getinverse()                      #gets the value of the inverse of the matrix stored in x with getinverse (function makeMatrix)                
      if(!is.null(inverse_matrix)) {                        #check that the value of inverse_matrix is not NULL (which means that the inverse has already been calculated)
          message("getting cached data")
          return(inverse_matrix)                            #returns the cache inverse
      }
      data <- x$get()                                       #otherwise, the inverse needs to be calculated so we store the value of the matrix in the variable data
      inverse_matrix <- solve(data, ...)                    #calculate the inverse of the matrix stored in the variable data
      x$setinverse(inverse_matrix)                          #set the variable x to the value of the calculated inverse
      inverse_matrix
}
