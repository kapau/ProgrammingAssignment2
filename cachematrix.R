
#You can run everything until the end to initialize values
#some test runs:
#cacheSolve(test)
#cacheSolve(test,d) << to test comparison
#cacheSolve(test,c) << to test comparison


#Clear all Variables
rm(list = ls())

#Create two invertible matrices
c <- rbind(c(1, -1/4), c(-1/4, 1))
d <- rbind(c(4,3),c(3,2))

#function that caches the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #initialize variables
  inverseMatrix <- NULL
  tempMatrix <- NULL
  
  #setting variables using <<-
  
  setOrigMatrix <- function(x){
    
    origMatrix <<- x
    if (is.null(tempMatrix)) {tempMatrix <<- x}
    
  }
  
  setInverseMatrix <- function(x) {
    
    inverseMatrix <<- x
    
  }
  
  setTempMatrix <- function(x) {
    
    tempMatrix <<- x
    
  }
  
  # retrieving variables 
  
  getInverseMatrix <- function(x) {
    
    inverseMatrix
    
  }
  
  getMatrix <- function() {
    
    origMatrix
    
  }
  
  getTempMatrix <- function() {
    tempMatrix 
  }
  
  # assigns list/menu of sub functions within makeCacheMatrix function
  
  list(setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix, getMatrix = getMatrix, getTempMatrix=getTempMatrix
       ,setOrigMatrix = setOrigMatrix, setTempMatrix = setTempMatrix)
  
}

#solves for inverse of matrix
cacheSolve <- function(x, y=c,...) {
  
  #set tempMatrix to Y. This will be compared later to origMatrix. 
  #if they are the same then it will load result using the cached solution
  x$setTempMatrix(y)
  
  # get inverseMatrix value
  a <- x$getInverseMatrix()
  
  #compare if matrix arg passed is identical to previous to check if a new matrix needs to be solved
  c <- x$getTempMatrix()
  d <- x$getMatrix()
  same <- identical(c,d)
  
  # if null then it will solve for the first time
  if (!is.null(a)) {
    print("checking if not the same")
    
    #if it is not null then check if passed matrix argument is the same  
    if(same) {
      
      print("Matrix is the same..no need to solve...retrieving from cache")
      x$getInverseMatrix()
      
    }
    
    #solve the new matrix and set passed matrix arg as the origMatrix
    else {
      
      #Solve inverse
      c <- solve(x$getTempMatrix())
      
      #Set new matrix as origMatrix
      d <- x$getTempMatrix()
      x$setOrigMatrix(d)
      
      print("Solving new inverse...because not the same orig matrix")
      x$setInverseMatrix(c)
      x$getInverseMatrix()
      
    }
    
  }
  
  # solving inverse for the first time and setting it as the cached solution
  else {
    
    print("Solving inverse...because inverse is NULL")
    c <- solve(x$getMatrix())
    x$setInverseMatrix(c)
    x$getInverseMatrix()
    
  }
  
}

# Assign makeCacheMatrix function to test and set initial matrix to C
test <- makeCacheMatrix(c)
test$setOrigMatrix(c)
