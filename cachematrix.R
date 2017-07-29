## functions to cach the inv of a matrix instead of recalculating it, 
## it check if the matrix did not change, if it changed, it recalculate the inverse

## function that create the matric object

makeCacheMatrix <- function(x = matrix()) {
      inv_m<-NULL
      change<-FALSE
      set <-function(y)
      {
        x<<-y
        inv_m<<-NULL
        change<<-TRUE
      }
     
      get <-function() x
      set_inv<-function(solve) 
      {
        inv_m<<-solve
        change<<-FALSE
      }
      get_inv<-function() inv_m
      list(set = set, change = change, get = get, set_inv = set_inv, get_inv = get_inv)

}
## function that return inverse, calculate it if needed only the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv_m<-x$get_inv()
      if(!is.null(inv_m) && !x$change)
      {
        print("getting cashed matrix")
        return(inv_m)
      }
      data <- x$get()
      inv_m <- solve(data,...)
      x$set_inv(inv_m)
      inv_m
}
