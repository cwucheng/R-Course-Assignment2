makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,  #make a list which contains 4 functions 
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#Assignment 2 
makeCacheMatrix <- function(x = vector,row=2) {
  mInverse<-NULL
  set <- function(y,row) {
    z <<- matrix( y, nrow=row, byrow=TRUE)
    if(nrow(x)!=ncol(x)) print("Error!The matrix should be square!")
    mInverse<-null
  }
  get <- function() matrix( x, nrow=row, byrow=TRUE)
  setInverse <- function(solve) mInverse<<- solve
  getInverse <- function() mInverse
  list(set = set, get = get,  #make a list which contains 4 functions 
       setInverse = setInverse,
       getInverse = getInverse)
}

#this program show to how to store and read matrix into cache, and calculate the
#inverse matrix 

#MakeCacheMatrix read matrix into cache
#cacheSolve calculate inverse matrix from cache 

makeCacheMatrix <- function(x = matrix()) {
  mInverse<-NULL
  
  if(nrow(x)!=ncol(x)) print("Error!The matrix should be square!")
  
  set <- function(y) {
    #z <<- matrix( y, nrow=row, byrow=TRUE)
    x<<-y
    mInverse<-null
  }
  get <- function() x #matrix( x, nrow=row, byrow=TRUE)
  setInverse <- function(solve) mInverse<<- solve
  getInverse <- function() mInverse
  list(set = set, get = get,  #make a list which contains 4 functions 
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  data <- x$get()
  mInverse <- solve(data)
  x$setInverse(mInverse)
  mInverse
  print(mInverse)
}

# use the 2 functions above 

list1<-makeCacheMatrix(matrix(1:4,2))
cacheSolve(list1)


  