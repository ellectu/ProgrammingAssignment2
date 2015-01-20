# Example of use
# source("cachematrix.R")
# m<-matrix(rnorm(100),nrow=10)
# mc<-makeCacheMatrix(m)
# class(mc) # should be "list"
# cacheSolve(mc) 
# mci<-cacheSolve(mc)
# round(mci%*%m,digits=10) # should be identity matrix


# Making a special matrix object that is able
# to store (cache) the inverted matrix
makeCacheMatrix <- function(x) 
{
    x_inv <- NULL
    # some simple checks: the data type of x
    if (class(x)!="matrix")
    {
        x<-NULL
        return(-2)
    }
    # matrix must be square
    else if (nrow(x)!=ncol(x))
    {
        x<-NULL
        return(-1)
    }
    
    # Setting the matrix data
    set <- function(mtrx)
    {
        # Yes, I know that in this assignment
        #  the matrix is always invertible, but....
        
        # some simple checks: the data type of x
        if (class(x)!="matrix")
            return(-2)
        # matrix must be square
        else if (nrow(x)!=ncol(x))
            return(-1)
        # you can add one more time consuming check -
        #  determinant of the matrix should not be 0
        # else if (abs(det(x))<10e-10) return(-3)
        else
        {       
            x<<-mtrx
            x_inv<<-NULL
            return(0)
        }
    }
    
    # Getting the matrix data
    get <- function() 
    {
        x
    }
    
    # Setting the inverted matrix that 
    #  should be stored and cached
    set_inv <- function(mtrx_inv)
    {
        x_inv<<-mtrx_inv
    }
    
    # Getting the stored inverted matrix
    # Null check needed (if was not stored)
    get_inv <- function()
    {
        x_inv            
    }
    
    # Returning the list of functions
    list (set = set, get = get, 
          set_inv = set_inv, get_inv=get_inv)
}

# returning the inverted matrix of the matrix object
#  created by makeCacheMatrix
cacheSolve <- function(x, ...) 
{
    # Trying to get the stored inverted matrix
    x_inv<-x$get_inv()
    if (!is.null(x_inv))
    {
        # Comment the line below for production
        message("cached inverse was used")
        # It has been saved earlier -
        #  returning the inverted matrix
        return(x_inv)
    }
    
    # We're getting to this lines only if matrix
    #  was not retrieved from cache
    x_inv<-solve(x$get(), ...)
    # Caching the value
    x$set_inv(x_inv)
    x_inv
}
