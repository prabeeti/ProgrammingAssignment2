

#Assignment 2
makeCacheMatrix <- function(x = matrix()) {
  a1<-NULL
  set<-function(y){
    x<<-y
    a1<<-NULL
  }
  get<-function() x
  setmtx<-function(solve) a1<<- solve
  getmtx<-function() a1
  list(set=set, get=get,
       setmtx=setmtx,
       getmtx=getmtx)
}

cacheSolve <- function(x=matrix(), ...) {
  a1<-x$getmtx()
  if(!is.null(a1)){
    message("get cache")
    return(a1)
  }
  matrix<-x$get()
  a1<-solve(matrix, ...)
  x$setmtx(a1)
  a1
}
