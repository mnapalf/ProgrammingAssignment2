## Put comments here that give an overall description of what your
## functions do
	## set the value of the matrix
   	## get the value of the matrix
	## set the value of the inverse
    	## get the value of the inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	## takes our input and assigns it to x in the parent environment of the function
	## m is set to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## retrieves the value of x. function with no arguments which expresses the value of x
        get <- function() x
	##calculates the inverse and stores in setinverse
        setinverse <- function(inverse) m <<- inverse
	## retrieves the value of the inverse, which is stored in getinverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function creates the inverse, if it has not already been calculated. 
## If calculated, takes the value from the cached memory and skips the calculation; 
otherwise, it calculates the inverse and sets its value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cachemean <- function(x, ...) {
	## retrieves the value of m
        m <- x$getinverse()
	## if not null (i.e. contains a value) skips the calculation and gets cached data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## if it has to calculate: gets the matrix (input=x)
        data <- x$get()
	## applies the function to calculate the inverse. Possible to pass extra args
        m <- solve(data, ...)
        x$setinverse(m)
	## autoprints the calculated inverse matrix
        m
}

}
