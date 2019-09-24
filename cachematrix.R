#The code below have been adjusted to change the functionality from vectors to matrices from the below source
#https://github.com/rdpeng/ProgrammingAssignment2
#The function makeCacheMatrix below takes as an input Matrix and stores it inside, you can change it by using sub
#function $set() or print it by $get().
#You can also get the inverse of an original matrix by $getsolve assuming or was calculated, as otherwise you will get 
#default NULL value; you can also set the inverse by $setsolve(here is inverse matrix) but for that we have next 
#function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


#This function takes as an input object of class makeCacheMatrix, and returns the inverse of the stored matrix
#In case the inverse was not yet calculated, it calculates and stores in the original workspace of the 
#makeCacheMatrix. In case it was previously calculated it prints "getting cached data" and returns the previously
#calculated inverse matrix. 

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
