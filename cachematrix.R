
## This function returns a list of 4 elemenets 
##which are basically functions and operate differently on a matrix x

makeCacheMatrix <- function(x = matrix()) {
        m=NULL
        
        set=function(z) {
                
                x<<-z
                m<<-NULL
                
        }
        
        get=function() x
        setResult=function(y) m<<-y
        getResult=function() m
        
        
        list(get=get,set=set,getResult=getResult,setResult=setResult)
}


## This function will firstly check wheter the inverse of the matrix is in the cache
##if it is so, then will just retrive the inverse from the cache, otherwise will calculate the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m=x$getResult()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m=solve(data,...)
        x$setResult(m)
        
        m
}
