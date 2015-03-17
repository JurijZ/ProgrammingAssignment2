
## In this Programming Assignment will take advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object.

## makeCacheMatrix() function is a closure. Closure maintains access to the environment in which it was created.
## It describes three things: functions, data variables and their scopes. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL        
  setmatrix <- function(y) {  
    ## Unlike the usual single arrow assignment (<-) that always assigns in the current environment, 
    ## the double arrow operator will keep looking up the chain of parent environments until it finds a matching name.
    ## Static parent environment and <<- make it possible to maintain state across function calls.
    x <<- y
    inv <<- NULL  
  } 
  getmatrix <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse      
  }
  getinverse <- function() {
    inv       
  }
  
  # Last expression in the function is returned by default.
  # Here we simply collect all our functions in a list for the future $ sign use:
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() function computes the inverse of the matrix and uses environment to store matrix and it's inverse. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve() will retrieve the inverse from the cache/environment variable. 
## If the matrix is new then cachsolve() will calculate new inverse and put it in a cache/environment

## cacheSolve() accepts two variables: the environment and the input matrix to calculate
cacheSolve <- function(E, mat) {
  
  cached_mat <- E$getmatrix() # cached matrix
  
  if (is.matrix(cached_mat) && is.matrix(mat) && dim(cached_mat) == dim(mat) && all(cached_mat == mat)) {
    # If the input matrix is the same as the one stored in the cache/environment then we can return cached inverse matrix
    
    print('inverse of the provided matrix is in the cache, returning')
    inv <- E$getinverse()
    if(!is.null(inv)) {              
      return(inv)  
    } 
  }
  else {
    # If the input matrix is new then we have to load the matrix into the environment, calculate it's inverse and load inverse into the environment
    
    print('inverse of the provided matrix is not yet cached, caching and returning')
    E$setmatrix(mat)       
    inv <- solve(mat)        
    E$setinverse(inv)        
    return(inv)
  }
}

####### Testing #######

# First I'm going to initialise my closure. 
# As a result we are going to get an empty environment E prepared to be used accross independant cacheSolve() calls.
# E it's a shared environment in other words
E <- makeCacheMatrix()

# Next I'm going to load a matrix and the environment into my caching function "cacheSolve(E, mat1)"
mat1 = rbind(c(4, 3), c(3, 2)) # input matrix
inverse = cacheSolve(E, mat1) # first time inverse will be calculated and cached
inverse = cacheSolve(E, mat1) # this time inverse will be taken from the cache

# And Finally I will load a different matrix to see how it will be cached on top of the preveous matrix
cat("\n")
mat2 = rbind(c(1, -1/4), c(-1/4, 1)) # new input matrix
inverse = cacheSolve(E, mat2) # matrix compare will trigger inverse recalculation and caching of both, matrix and it's inverse
inverse = cacheSolve(E, mat2) # and second time inverse will be taken from the cache
