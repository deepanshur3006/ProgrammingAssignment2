## makeCacheMatrix function has 4 nested functions to 1. Set the Original Matrix (setmatrix)
## 2. Get the Original Matrix (getmatrix)
## 3. Set the Inverted Matrix (setinvertedmatrix) - Called by cacheSolve()
## 4. Get the Inverted Matrix (getinvertedmatrix)

## cacheSolve function checks if the inverse of a matrix has already been calculated. If not,
## cacheSolve calculates the inverse and passes the result to setinvertedmatrix

## makeCacheMatrix takes a square matrix as input. Please note argument name x changed to
## originalmatrix for better understanding.

makeCacheMatrix <- function(originalmatrix = matrix()) {

  ## The invertedmatrix variable is set to NULL in beginning  
  invertedmatrix<-NULL 

##Checking for square matrix  
if(nrow(originalmatrix)!=ncol(originalmatrix)) { 
  
  print('The matrix is not a sqaure matrix. Please provide a square matrix')
  
}else{
    setmatrix<-function(inputmatrix){
      
    ## The original matrix value is taken based on input matrix
    originalmatrix<<-inputmatrix
    
    ## The inverted matrix is set to NULL as the original matrix has been changed
    invertedmatrix<<-NULL
    }
    
    ## Getting the values in original matrix
    getmatrix<-function() originalmatrix
    
    ## Setting the inverted matrix on call by cacheSolve funtion. Argument is inverse of 
    ## original matrix calculated by solve funtion
    setinvertedmatrix<-function(solvedinvertedmatrix) invertedmatrix<<-solvedinvertedmatrix
    
    ## Returning the inverted matrix
    getinvertedmatrix<-function() invertedmatrix
  
    ## 4 Nested functions as list
    list(setmatrix=setmatrix,getmatrix=getmatrix,
       setinvertedmatrix=setinvertedmatrix,getinvertedmatrix=getinvertedmatrix)
}
  
}


## The below function checks if the inverse of original matrix has already been calculated
## If not, it calculates the inverse using solve function and passes as argument to setinvertedmatrix
## function in makeCacheMatrix function.

## If the inverse is already calculated, the cached value is retrieved and printed

cacheSolve <- function(originalmatrix, ...) {
  
  ## Retrieving the invertedmatrix
  invertedmatrix<-originalmatrix$getinvertedmatrix()
  
  ## If it exists as cached value, it is printed
  if(!is.null(invertedmatrix)){
    message('getting cached data')
    return(invertedmatrix) 
  }
  
  ## If inverse matrix has not been calculated before, it is now calculated
  tempmatrix<-originalmatrix$getmatrix()
  invertedmatrix<-solve(tempmatrix)
  
  ## The setinvertedmatrix function is called 
  originalmatrix$setinvertedmatrix(invertedmatrix)
  
  ## Inverted matrix is printed
  invertedmatrix
}
