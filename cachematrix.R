## Put comments here that give an overall description of what your
## functions do

#I started from the example of the makeVector() and cachemean() functions to write the script

## makeCacheMatrix takes a matrix as input and outputs a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {    # the x variable is created as argument of the function
  inverse <- NULL                              # initializes the variable "inverse"
  set <- function(y) {                         # 1st "subfunction" of makeCacheMatrix: assignes the values ->
    x <<- y                                    # -> of y to the x variable in the makeCacheMatrix environemt
    inverse <<- NULL                           # "empties" the inverse variable in the makeCacheMatrix environment
  }
  get <- function() x                          # 2nd "subfunction"; get the value of x from the makeCacheMatrix environment
  setinverse <- function(inv) inverse <<- inv  # 3rd "subfunction"; takes inv as input and assigns it to the inverse variable in the parent environment
  getinverse <- function() inverse             # 4th "subfunction"; get the value from the inverse variable in the parent environment
  c(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # builds a vector with the functions 1-2-3-4 and sets their names
}

## CacheSolve requires x variable as input in the form produced by makeCacheMatrix. Computes the inverse matrix or outputs the matrix previously calculated 
#if it is already stored in the parent environment of makeCacheMatrix

CacheSolve <- function(x) {
  inversevalue <- x$getinverse()             # fetches the data using the getinverse function in the x variable and sets it to inversevalue 
  if(!is.null(inversevalue)) {               # evaluates if the the inversevalue contains an object
    message("Getting data from cache")       # outputs the message "Getting data from cache" and
    return(inversevalue)                     # outputs the inverse matrix calculated previously
  }                                          # if inversevalue is empty, the function solves the matrix inversion
  data <- x$get()                            # the matrix to be inverted is fetched from the makeCacheMatrix output
  inversevalue <- solve(data)                # the matrix is solved (with the solve function) and stored in the inversevalue object
  x$setinverse(inversevalue)                 # the inverse is assigned to the "inverse" object in the makeCacheMatrix output 
  message("Computing inverse")               # prints the message that the inverse was computed for the first time
  inversevalue                               # returns the value of the inverse matrix just calculated
}

#TEST
w1 <- c(1,2)
w2 <- c(1,0)
matrix1 <- rbind(w1,w2)

v1 <- c(1,2,0)
v2 <- c(1,0,-1)
v3 <- c(0,1,1)
matrix2 <- rbind(v1,v2,v3)
