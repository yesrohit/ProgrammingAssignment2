## Two functions have been written: 'makeCacheMatrix' which takes a matrix as an input and the 'cacheSolve' function
## which takes an instance of the makeCacheMatrix function to see if it already has an inverse calculated. If it does, 
## it reports the value of the stored inverse otherwise calculates it and reports the inverse. 

## Write a short comment describing this function
## makeCacheMatrix: This is the function called when initializing a matrix. The matrix may be initialized as follows:
## Eg. Mat1<-makeCacheMatrix(matrix(rnorm(9),3,3))
## This will generate a Mat1 function list containing the 3x3 matrix in its environment. 
## Mat1 is list of 4 functions: set, get, setinverse and getinverse. 
## Set() will change the matrix data or set it to the given argument and make the inverse NULL in the parent environment. Eg. Mat1$set(matrix(rnorm(9),3,3))
## Get() will retrieve the matrix data for output. Eg. Mat1$get()
## Setinverse() will set the inverse of the matrix to the given argument: Eg. Mat1$setinverse(Inverse)
## Getinverse() will retrieve the value of the stored inverse: Eg. Mat1$getinverse()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function: cacheSolve
## It takes an instance of makeCacheMatrix function and checks if it already has an inverse associated with it, if it does,
## it calls the getinverse function to get the 'inv' (inverse). If not, it gets the matrix data and calculates the inverse 
## using the solve function. 
## Eg. cacheSolve(Mat1)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
	inv <- x$getinverse() 
	
	##Check if inverse exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
	
	##If it doesnt exist, get the matrix data and calculate inverse
    data <- x$get()
    
    ## Code for solving inverse of a matrix
    inv <- solve(data,...)
    
	##Set the value of the inverse of the matrix
    x$setinverse(inv)
    inv
}
