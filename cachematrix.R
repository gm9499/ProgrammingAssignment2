## Write two functions,"makeCacheMatrix" and "cacheSolve" to cache  the inverse of the matrix


## makeCacheMatrix is a function that can cache  the inverse of the input
makeCacheMatrix <- function(x = matrix()) {
+  inv <- NULL
+  set <- function(y) {
+    x <<- y
+    inv <<- NULL
+  }
+  get <- function() x
+  setinv <- function(inverse) inv <<- inverse
+  getinv <- function() inv
+  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cachesolve is a function that can compute the inverse of the matrix returned by makeCacheMatrix .
  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+  inv <- x$getinv()
+  if(!is.null(inv)) {
+    message("getting cached result")
+    return(inv)
+  }
+  data <- x$get()
+  inv <- solve(data, ...)
+  x$setinv(inv)
+  inv
}
##Check Program
> m<-matrix(rnorm(16),4,4)
> m1<-makeCacheMatrix(m)
> cacheSolve(m1)
           [,1]       [,2]       [,3]        [,4]
[1,] -0.5085842 -0.3596076 0.01316602  0.07122498
[2,]  0.5495866  0.6219160 0.65900936 -0.27386759
[3,]  0.1351830  0.5012417 0.18645261  0.43152171
[4,] -0.0163095  0.3957428 0.04302697 -0.23130732

