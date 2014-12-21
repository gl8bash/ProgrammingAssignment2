
# The functions take an input matrix 'x' and return its inverse.
# If the matrix is already in cache, the functions retrive the value stored 
#  in cache thus avoiding expensive re-computation.


#---------------------------------------------------------------------
# Function : makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

# Input is a matrix
makeCacheMatrix <- function(x = matrix()) {
  # sets inverse equal to an empty vector 
  inv <- NULL
  
  # This piece of code is not needed however useful for debugging purposes
  # Reassign x during de-bugging
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Store the input varible as a list 
  get <- function() x
  
  # Assign the value of the inverse calculated
  setinverse <- function(inverse) inv <<- inverse
  
  # Store the value of the inverse as list
  getinverse <- function() inv
  
  # Assign Values
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


#-------------------------------------------------------------------------------------------
# Function : cacheSolve

# This function gets the inverse from cache if it is already computed.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
# If a new imput matrix is provided, the function calculates the inverse and stores in cache

cacheSolve <- function(x, ...) {
  # Access the inverse already in cache for the same input x
  inv <- x$getinverse()
  
  # Return the inverse value if not null for same x
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise get the data from the get varable in list makeCacheMatrix
  # <variable>$get
  data <- x$get()
  
  # Calculate the inverse
  inverse <- solve(data)
  
  # Set the vale of calculated inverse back in cache
  x$setinverse(inverse)
  
  # Print the inverse
  inverse
}


#Test Cases
#--------------

## mat1 <- matrix(1:4, nrow = 2 , ncol = 2)
## print (mat1)

## cm1 <- makeCacheMatrix(mat1)
## print(cm1)

## matinv1 <- cacheSolve(cm1)
## print (matinv1)

## matinv1_from_cache <- cacheSolve(cm1)
## print(matinv1_from_cache)


## mat2 <- matrix(c(1,2,3,0,1,5,5,6,0), nrow = 3 , ncol = 3, byrow=TRUE)
## print (mat2)

## cm2 <- makeCacheMatrix(mat2)
## print(cm2)

## matinv2 <- cacheSolve(cm2)
## print (matinv2)

## matinv2_from_cache <- cacheSolve(cm2)
## print(matinv2_from_cache)

## cacheSolve(cm1)
## cacheSolve(cm2)






