# Caching the Inverse of a Matrix. 
# create a matrix with x data, nrow, and ncol.
# create four functions and two objects in the parent environment makeCacheMatrix
makeCacheMatrix <- function(x, nrow, ncol) {         # x is initialized with other two arguments nrow and ncol 
         dim(x) <- c(nrow, ncol)                     # create a matrix with x data.
         i <- NULL                                   # i is initialized as NULL
         set <- function(y) {                        # create a setter func for the vector x
             x <<- y                                 
             i <<- NULL
         }
         get <- function() x                         # create a getter function for the vector x         
         setinverse <- function(solve) i <<- solve   # create setinverse environment for function 'solve'
         getinverse <- function() i                  # create getinverse environment to retrieve data object i
         list(set = set, get = get,                  # give name to each function so that we can use $ to extract function(s).
              setinverse = setinverse,
              getinverse = getinverse)
}
# The following function checks and computes the inverse i of the vector x when it is necessary
cacheSolve <- function(x,...) {                      # initializing x
         i <- x$getinverse()                         # retrieve the inverse i of x 
         if(!is.null(i)) {                           # checking the validity of i
             message("getting cached data")
             return(i)
         }
         data_i <- x$get()                           # if i is invalid, computation starts from obtaining the get() func in the parent environment
         i <- solve(data_i, ...)                     # computing the inverse of x and assign it to i
         x$setinverse(i)                             # set the inverse i in the setinverse function
         i
}
