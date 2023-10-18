## Matrix object with 2 properties - original and inverted matrix
## (assume that the matrix supplied is always invertible)
makeCacheMatrix <- function(x = matrix()) {
    set <- function(x) {
        original_matrix <<- x
        inverted_matrix <<- NULL
    }
    get <- function() original_matrix
    
    set_inverted <- function(inverted) inverted_matrix <<- inverted
    get_inverted <- function() inverted_matrix
    
    set(x)
    
    list(set = set, get = get, set_inverted = set_inverted, get_inverted = get_inverted)
}


## Inverse matrix ONLY if the inverted version does NOT exist!
cacheSolve <- function(x, ...) {
    # Get cached value
    inverted_matrix <- x$get_inverted()
    if(!is.null(inverted_matrix)) {
        message("Getting cached inverted matrix")
        return(inverted_matrix)
    }
    
    # Create value if cached value does not exist
    original_matrix <- x$get()
    inverted_matrix <- solve(original_matrix, ...)
    
    x$set_inverted(inverted_matrix)
    
    inverted_matrix
}

# Test created functions
cache_matrix <- makeCacheMatrix(matrix(c(2, 1, 1, 3), nrow = 2))
print(cache_matrix$get())

cacheSolve(cache_matrix)
cacheSolve(cache_matrix)
