## Program takes constructer function of makeCacheMatrix and obtain data in this function and calculates inverse of matrix in cacheSolve function and place data in the variable.


## Function take numeric matrix as input and stores it in the variable.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmat <- function(y){
                x <<- y
                i <<- NULL
        }
  ## to return value of matrix      
        getmat <- function() x
  
  ## to set the value of inverse of matrix      
        setinv <- function(inverse) i <<- inverse
  
  ## to cache the value of inverse of matrix
        getinv <- function() i
        list(setmat = setmat,
             getmat = getmat,
             setinv = setinv,
             getinv = getinv)
        
}


## function to first cache the inverse if already calculated, otherwise calcualte using gniv function from MASS package

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmat()
        i <- ginv(data)
        x$setinv(i)
        ## Return a matrix that is the inverse of 'x'
        i
}

## Added the ginv function code from MASS package for where package not installed, it will run smoothly.
ginv <- function (X, tol = sqrt(.Machine$double.eps)) 
{
        if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
                stop("'X' must be a numeric or complex matrix")
        if (!is.matrix(X)) 
                X <- as.matrix(X)
        Xsvd <- svd(X)
        if (is.complex(X)) 
                Xsvd$u <- Conj(Xsvd$u)
        Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
        if (all(Positive)) 
                Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
        else if (!any(Positive)) 
                array(0, dim(X)[2L:1L])
        else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
                                                           t(Xsvd$u[, Positive, drop = FALSE]))
}