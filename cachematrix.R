

## Function:makeCacheMatrix - it creates an object of type list with the following functions
## 1. makeCacheMatrix$set which set the matrix with input data as matrix
## 2. makeCacheMatrix$set which gets the value of matrix inside
## 3. makeCacheMatrix$setinv which sets inverted matrix
## 4. makeCacheMatrix$getinv which gets inverted matrix inside 

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(solved) invmat <<- solved
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function:cacheSolve - This function makes takes an object of type list which is output 
## of above function. If its a new matrix then the inverted matrix is calculated newly. If
## its the existing matrix data and inverted matrix is already calculate, it returns the 
## cached inverted matrix. It saves time for repeated work of calculating inverted matrix
## for same matrix data.

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}

# Example usage :
# take a 3X3 matrix which is invertible
# > a <- matrix(c(8,3,1,8,2,4,0,1,2),3,3)
# > a
# [,1] [,2] [,3]
# [1,]    8    8    0
# [2,]    3    2    1
# [3,]    1    4    2
# > det(a)
# [1] -40
#
# > cmat <- makeCacheMatrix(a)
# > cmat <- makeCacheMatrix(a)
# > cacheSolve(cmat)
#               [,1] [,2] [,3]
# [1,] -2.775558e-17  0.4 -0.2
# [2,]  1.250000e-01 -0.4  0.2
# [3,] -2.500000e-01  0.6  0.2
# when you call it again, it will give you cached data
# > cacheSolve(cmat)
# getting cached data
#               [,1] [,2] [,3]
# [1,] -2.775558e-17  0.4 -0.2
# [2,]  1.250000e-01 -0.4  0.2
# [3,] -2.500000e-01  0.6  0.2

