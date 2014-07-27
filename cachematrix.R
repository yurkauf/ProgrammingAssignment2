## Implemented code which produce inversed matrix from the given square matrix and then cache the result.
##Example of how to use functions below
##>      s<-makeCacheMatrix(c(1, 5, 5, 2, 3, 4, 2, 2, 8))
##> next code returns matrix created from the vector 
##       s$get_matrix() 
## next code calculates inversed matrix 
##       cacheSolve(s)
## after one execution function will start returning cached result
## next code setup a differnt matrix
##       s$set_matrix(c(1, 5, 5, 2, 3, 4, 2, 2, 11))
## next calculates again inversed matrix, but because new matrix have been created
## variable storing hache was reset to null and program has to calculate new inversed matrix
##       cacheSolve(s)


##makeCascheMatrix creates square matrix from the numeric vector and also provides the cach
##for caching matrix. Result of the function is a list of the functions 

makeCacheMatrix <- function(x=numeric()) {

       create_matrix <- function() {
              l <- length(x)
              if(sqrt(l) != round(sqrt(l), 0)) {
                     message("Cannot generate square matrix from given vector")
                     return(-1)
              }    
              matrix(x, sqrt(l), sqrt(l))
              
       }
       mx <- create_matrix()
       mxinv <- NULL
       set_matrix <- function(y) {
              x <<-y
              mx <<- create_matrix()  
              mxinv <<- NULL
       }
       get_vector <- function() x 
       get_matrix <- function() mx
       set_inverse_matrix <- function(mxinv_new) mxinv <<- mxinv_new
       get_inverse_matrix <- function()mxinv
       list(set_matrix=set_matrix, get_vector = get_vector, set_matrix = set_matrix, get_matrix = get_matrix, 
            set_inverse_matrix = set_inverse_matrix, 
            get_inverse_matrix = get_inverse_matrix)
}


## function cacheSolve calculates inversed matrix and cach result. Every time before 
## calculating inverse matrix it checks if cached inversed matrix already exists and then 
## returns cached version if it exists 
cacheSolve <- function(x,...) {
       mat_inv = x$get_inverse_matrix()
       if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        xmat <- x$get_matrix()
        mat_inv <- solve(x$get_matrix())
        x$set_inverse_matrix(mat_inv)
        mat_inv
}

 