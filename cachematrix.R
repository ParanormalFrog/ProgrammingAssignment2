#These functions allows storage of inverse matrices, instead of having to calculate them each time 


#this functions converts a matrix to a list of functions, that allow retrieval and edit of the input matrix by using get and set, as well the inverse matrix by the two remaining functions.


makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) im <<- matrix
        getmatrix <- function() im
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
#this function evaluates whether an inverse matrix has been calculated for the input matrix. If it has, that matrix is retrieved. If it hasn't. That matrix is calculated and cached. It takes as argument the list creatded from the previous function
cachesolve <- function(x, ...) {
        im <- x$getmatrix()
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data)
        x$setmatrix(im)
        im
}
