## In this assignment we want to cache the inverse of the matrix which leads to reduce the computation.

##makeCacheMatrix creates a special "matrix" object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
	Inverse <- NULL
	Matr<-function(y){
		x <<- y
		Inverse <- NULL
			}
	InvMatr <- function() x
	setInverse <-function(Inv) Inverse <<-Inv
	getInverse <- function() Inverse
	list(Matr = Matr, InvMatr = InvMatr,
	setInverse = setInverse,
	getInverse= getInverse)
}


## cacheSolve function computes the invers of the returned matrix from the makeCacheMatrix function if it has not been calculated, otherwise, it retrives the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
        	message("retriving cache inverse")
			return(Inverse)
        }
        data <-x$InvMatr()
        Inverse<-solve(data,...)
        x$setInverse(Inverse)
        Inverse
}


