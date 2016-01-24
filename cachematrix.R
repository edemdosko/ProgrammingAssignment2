###################################################################
#The following two functions are meant to be an application of the 
#the lexical scpoing rules in R. One will create a matrix that ca-
#ches its inverse--makeCacheMatrix, and the other will compute the
#inverse of the matrix
###################################################################


## function that creates a matrix a caches it
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x        
        set_solve <- function(solve) s <<- solve
        get_solve <- function() s
        list(set = set, get = get,
             set_solve = set_solve,
             get_solve = get_solve)
}


##function to compute the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$get_solve()
        if(!is.null(s)) {
                message("getting cached matrix")
                return(s)
        }
        my_matrix <- x$get()
        s <- solve(my_matrix)
        x$set_solve(s)
        s
}
