###Here there are 2 functions to be able to cache the inverse of a matrix
###so it doesn't have to be computed many times if it didn't change.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 					#Creates a variable inv with NA value.
	set <- function(y) { 				#Creates a function called set.
                x <<- y 				#x will be replaced in the parent level by y.
                inv <<- NULL 				#m will be replaced by NA.
        }
	get <- function() x 				#Gets x, even if doesn't exist yet.
	setinv<- function(solve) inv <<- solve 	#Sets the inverse matrix.
	getinv <- function() inv 			#Get the inv, even if doesn't exist yet.
	list(set = set, get = get,			#Makes a list of the elements defined
             setinv = setinv,				#within the function.
             getinv = getinv)


}


## cacheSolve calculates the inverse of the matrix returned by 
## the function makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinv() 				# Return a matrix that is the inverse of 'x'.
        if(!is.null(inv)) {				#If the inverse has already been calculated
                message("getting cached data")	#it is retreived from the cache, and a message 
                return(inv)				#is printed.
        }
        data <- x$get()					#If it hasn't been calculated, it is done so.
        m <- solve(data, ...)
        x$setinv(inv)					#Stores the value in the cache for a next time.
        return(inv)		
}
