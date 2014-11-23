## This code catches a matrix's inverse.  If the inverse
## is already calculated, then it retrieves the inverse 
## from the cache, saving valuable time, as the even most
## optimized matrix inversion algorithm runs at
## t~ O(n^2.4)


## This function creates a special matrix that can 
## cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize value of matrix to NULL
  m_inv<-NULL
    ##use <<- to make another environment for cache
  set<-function(y){
    x<<-y
      ##return  the inverse to null, stored in cache 'y'
    m_inv<<- NULL 
  }
    ##gets the matix
  get<- function()x
    ##uses solve to find the inverse
  set_inv<-function(solve) m_inv<<-solve
    ##gets inverse
  get_inv<-function()m_inv
    ##returns a list as input for cacheSolve()
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## This function first checks to to see if the inverse
## is already been calculated and in the cache from the list returned by 
## makeCacheMatrix.  If so  it returns the inverse from the cache, 
##if not, it returns the inverted matrix by the 'solve()' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  m_inv<-x$get_inv()
    ##The next lines check to see if the first function has a cache
    ## and if so it uses that (and prints a message)
  if(!is.null(m_inv)){
    message("Getting cached Data")
    return(m_inv)
  }
    ##If not, it uses the 'get' from above function to define matrix
  data<-x$get()
    ##solves the inverse of matrix
  m_inv<-solve(data,...)
    ##sets this as the inverse and then returns the inverse
  x$set_inv(m_inv)
  m_inv
}
