## The first function is a function that takes as input a matrix, and turns it
## into a particular kind of matrix, one of the class makeCacheMatrix.
## As an instance of that class, it has several features, in the form
## of functions. Here a makeCacheMatrix has four functions associated with it.

## The second function takes a matrix of class makeCacheMatrix as input and 
## returns its inverse.  


makeCacheMatrix <- function(x = matrix()) {
## makeCachematrix takes a matrix as input and associates four functions
## with this new object. The function set() is used to change the 
## values of the non-inverted matrix. The function get() returns only the 
## noninverted matrix. setinverse() is used to give a value to the object 
## 'inverse', which is the inverse of interest. This is done in such a way 
## that 'inverse' is always defined within the context of the object of 
## the class makeCacheMatrix. The function getinverse() simply returns the 
## value of object m. 
        inverse <- NULL # by default no inverse is associated with a new matrix.
        set <- function(y) {
                x <<- y  # change the matrix
                inverse <<- NULL # with a new matrix we have no inverse yet.
        }
        get <- function() x  # return the matrix
        setinverse <- function(solution) inverse <<- solution
        # associate a computed inverse with the object
        getinverse <- function() inverse
        # return the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {   # evaluate whether there already is one
                message("getting cached inverse")
                return(inverse)
        }
        message('computing inverse')  # if not, then try computing it
        data <- x$get() # take the original matrix
        inverse <- solve(data, ...) # compute its inverse
        x$setinverse(inverse) # associate it with the matrix object
        inverse # and return it to the console
}
