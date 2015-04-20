##############################################################################################################
# Matrix inversion can be time-consuming, especially if it is going to be done inside a loop.                #
# Instead of computing it repeatedly, one can cache the inverse of a matrix once and call it when necessary. #
# The pair of functions presented below calculate and cache the inverse of a matrix.                         #
# When appropriate, the inverse matrix is retrieved from the cache, and not recalculated.                    #
##############################################################################################################


# makeCacheMatrix creates a list of functions that can cache a matrix inverse. #

makeCacheMatrix <- function(original_matrix = matrix()) {
        inverse_matrix <- NULL
        # original_matrix <- input
        # inverse_matrix <- NULL
        set <- function(input) {
                original_matrix <<- input
                inverse_matrix <<- NULL
        }
        get <- function() original_matrix
        # inverse_matrix <- calculated_i_mtx
        set_i_mtx <- function(calculated_i_mtx) inverse_matrix <<- calculated_i_mtx
        get_i_mtx <- function() inverse_matrix
        list(set = set, get = get, set_i_mtx = set_i_mtx, get_i_mtx = get_i_mtx)

}


# cacheSolve checks if the inverse matrix has already been calculated. If so, it returns the inverse value from the cache. #
# If the inverse has not been calculated, then cacheSolve calculates it and store it in the cache (using makeCacheMatrix). #

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$get_i_mtx()
        if(!is.null(inverse_matrix)) {
                message("inverse matrix already calculated. getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$set_i_mtx(inverse_matrix)
        inverse_matrix
}
