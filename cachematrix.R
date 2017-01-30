
## There are two variables in the function
# x being the matrix to be processed
# m being the result to be saved

makeCacheMatrix <- function(x = matrix()) {
# define the result m
    m <- NULL
    
# set up value assignment in global enviroment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
# contant value function to retrive value of x
    get <- function() x
    
# assign the value of inverse to m
    setinv <- function(inv) m <<- inv
    
# retrive the value of m
    getinv <- function() m
    
# this function returns a list that contains 4 elements
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the input of this function needs to be makeCacheMatrix
# and return the inverse of this input matrix

cacheSolve <- function(x, ...) {
# x$getinv is to call the getin from makeCacheMatrix
# and check if it has been calculated before
    m <- x$getinv()
    if(!is.null(m)) {
    # if m is no null then it shows the inverse matrix has been determined
    # so return its cached value
        message("Getting cached data")
        return(m)
    }
# otherwise, call the get function to retrive the value of matrix
    data <- x$get()
    # calculate the inverse matrix by solve()
    m <- solve(data, ...)
    # call setinv function to store the calculated value in x
    # so next time cacheSolve is called, x already has the result due to if logic previously
    # finally return the result
    x$setinv(m)
    m
}

temp_matrix <- matrix(data = c(1,5,9,2,8,10,9,7,6), nrow = 3, ncol = 3)
cachedMatrix <- makeCacheMatrix(temp_matrix)
cacheSolve(cachedMatrix)
cacheSolve(cachedMatrix)
