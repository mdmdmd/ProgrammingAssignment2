## These functions create a matrix and caluculate it's inverse

## The makecacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	## Construct list of functions
	list(set=set, get=get,
		setmatrix=setmatrix,
		getmatrix=getmatrix)
}


cacheSolve <- function(x, ...) {
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
	m<-x$getmatrix()
	##  If m has been set, retrieve from cache and return from function
    if(!is.null(m)){
		message("getting cached data")
		return(m)
    }
    matrix<-x$get()
    ## Invert matrix
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
		

