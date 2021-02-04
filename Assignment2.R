#Assignment 2 

#this program show to how to store and read matrix into cache, and calculate the
#inverse matrix 

#MakeCacheMatrix read matrix into cache


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

#cacheSolve calculate inverse matrix from cache 
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
mInversecacheSolve(list1)
#test if the inverse matrix is correct 
mInverse%*%matrix(1:4,2)

  
