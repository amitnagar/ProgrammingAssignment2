## Function allows user to create a matrix and computes its inverse.  The  
## inverse is cached to improve performance during multiple retrievals. 

## The function makeCacheMatrix creates a special matrix object and provides
## set/get methods to store and retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set<-function(v, nrow, ncol){
             myMatrix<<-matrix(v,nrow=nrow, ncol=ncol)
             inv<-NULL
        }
        get<-function() myMatrix
							        
        ##sets the inverse
        setInverse<-function(inverse) inv<<-inverse 
										        
        ##returns the calculated inverse
        getInverse<-function() inv

										        list(set=set, get=get, 
             setInverse=setInverse, getInverse=getInverse)
}


## The function cachSolve returns a cached instance of inverse of given matrix. ## If cached instanced does not exist then it first calculates the inverse and
## then sets it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if (!is.null(inv)){
	     message("getting cached inverse")
	     return(inv)
	}
						        
	##if not cached, first get the matrix
	matrix<-x$get()
	inv<-solve(matrix)
										        x$setInverse(inv)
	inv
}
