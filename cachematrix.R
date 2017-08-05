## We are implementing two functions 1: makeCacheMatrix which will create and store a matrix and its inverse 
##                                   2: cacheSolve function which will take a makecacheMatrix function as a parameter then
##                                      checks whether the inverse is already computed, if so it will return the stored inverse,
##                                      otherwise it will calculate the inverse.

## this function will take a matrix as a parameter , and will return a list of four function (set : to set the value of the matrix
## get : to return the value of the matrix , setinv : to set the inverse of the matrix , getinv : to return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv=NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    
    get<-function() x
    setinv<-function(minv) inv<<-minv
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv) 
}


## this function will take a makeCacheMatrix as a parameter and then check for the inverse , if it is already calculated the it
## will be retuend , otherwise it will be calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
    if(!is.null(inv)){
        message("Getting cahced inverse")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
