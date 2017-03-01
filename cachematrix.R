
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                  # Holds values of inverse matrix
  set <- function(y){             #Define function that will set matrix value
    x<<-y                         #Value of matrix
    inverse <<-NULL               # Ensure that inverse stays empty after use
  }
  get <- function() x             #Return value of matrix
  
  setinverse <- function(inv) inv <<- inv #Asign value of inverse 
  getinverse <- function() inverse #Gets value of inverse matrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)   #Ensures that $ operator works
}

cacheSolve <- function(x, ...) {
  inverse <- x$getinvverse()      #CHeck for matrix x in cache
  if(!is.null(inverse)) {         #If the matrix is already in cache returns inverse
    message("Computing")
    return(inverse)
  }
  data <- x$get()                 #For a new matrix compute inverse
  inverse <- solve(data)          # Stores new inverse matrix in cache
  x$setinverse(inverse)
  inverse      
}
