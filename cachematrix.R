## The two functions makeCacheMatrix and cacheSolve are used to create a 
## special object that stores a matrix and cache's its inverse

## makeCacheMatrix returns a list of functions that sets the value of the
## matrix, gets the value of the matrix, sets the value of the inverse, and 
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <- function(y){
                x<<- y
                i<<- NULL
        }
        get<- function()x
        setinverse<-function(solve) i<<-solve
        getinverse<-function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix. It first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse 
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting catched data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setinverse(i)
        i
}
