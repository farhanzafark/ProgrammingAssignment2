## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y)
        {
                x<<-y
                inverse<<-NULL
        }
        get<-function() {x}
        setinverse<-function(inv) {inverse <<- inv}
        getinverse<-function() {inverse}
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function takes in the results of the makeCacheMatrix function (a list) and 
## uses those to get the cached inverse, if it exists. Otherwise, it finds out the
## inverse of the matirx and caches it.

cacheSolve <- function(x, ...) {
        ## Get the value stored in the cache.
        val_inv<-x$getinverse()        
        if(!is.null(val_inv)){
                ## If the stored value is not NULL, display the value.
                message("getting cached data")
                return(val_inv)
        }
        ## Get the sotred matrix
        data<-x$get()
        ## Find the inverse using the solve() function
        val_inv<-solve(data,...)
        ## Set the value of inverse in cache
        inv<-x$setinverse(val_inv)
        ## Return the calculated inverse
        val_inv
}

## Results
## > x<-matrix(rbind(c(2,3),c(2,2)),2,2)
## > x
## [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > test<-makeCacheMatrix(x)
## > test2<-cacheSolve(test)
## > test2
## [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
