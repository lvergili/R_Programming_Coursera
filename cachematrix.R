# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. Two functions are defined below.
#     First function creates a special "matrix" object that can cache its inverse.
#     Second one computes the inverse of the special "matrix" created by 
#   makeCacheMatrix function. If the inverse has already been calculated, then it should reclaim the 
#   inverse from the cache.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function computes the inverse of the matrix. It checks first if
# the inverse has already been computed. If not, it computes the inverse, sets the value 
#in the cache via setinverse function.If it has already been calculated, it gets the result 
# and skips the computation part.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## **************************Testing my function:***************************
## > myMatrix<-makeCacheMatrix(matrix(1:16,nrow=4,ncol=4))
## > myMatrix$get
## > myMatrix$get()
##        [,1] [,2] [,3] [,4]
##  [1,]    1    5    9   13
##  [2,]    2    6   10   14
##  [3,]    3    7   11   15
##  [4,]    4    8   12   16

## No cache in the first run
##  > cacheSolve(myMatrix)
##  Error in solve.default(data) : 
##  Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
#**********This error means matrix is not invertable(Det|myMatrix|=0)**********

#######################SECOND TRY#############
# > x<-c(2,3,4,5,8,3,5,6,12,21,19,43,21,11,45,90)
# > myMatrix<-makeCacheMatrix(matrix(x,ncol=4,nrow=4))
# > myMatrix$get()
# [,1] [,2] [,3] [,4]
# [1,]    2    8   12   21
# [2,]    3    3   21   11
# [3,]    4    5   19   45
# [4,]    5    6   43   90
# > cacheSolve(myMatrix)
# [,1]        [,2]         [,3]        [,4]
# [1,] -0.222443957  0.12233087  0.595423402 -0.26075966
# [2,]  0.177143617 -0.03013371 -0.050422404 -0.01243930
# [3,]  0.008248520  0.04929156 -0.083083882  0.03359276
# [4,] -0.003392536 -0.02833766  0.009978048  0.01037717
# > cacheSolve(myMatrix)
# getting cached data.
# [,1]        [,2]         [,3]        [,4]
# [1,] -0.222443957  0.12233087  0.595423402 -0.26075966
# [2,]  0.177143617 -0.03013371 -0.050422404 -0.01243930
# [3,]  0.008248520  0.04929156 -0.083083882  0.03359276
# [4,] -0.003392536 -0.02833766  0.009978048  0.01037717
# > 
