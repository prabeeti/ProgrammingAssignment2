

#Assignment 2 for Lexical Scoping
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)
makeCacheMatrix <- function(x = matrix()) {
  check_inverse<-NULL
  set<-function(y){
    x<<-y
    check_inverse<<-NULL
  }
  get<-function() x
  set_inverse<-function(solve) check_inverse<<- solve
  get_inverse<-function() check_inverse
  list(set=set, get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 


cacheSolve <- function(x=matrix(), ...) {
  check_inverse<-x$get_inverse()
  if(!is.null(check_inverse)){
    message("geting cache data")
    return(check_inverse)
  }
  my_matrix<-x$get()
  check_inverse<-solve(my_matrix, ...)
  x$set_inverse(check_inverse)
  check_inverse
}
#Take an example of Invertible matrix
#Invertible matrix -If the square matrix has invertible matrix or non-singular if 
#and only if its determinant value is non-zero. Moreover, if the square matrix A is 
#not invertible or singular if and only if its determinant is zero. 
#Obtain the determinant of the given matrix. Since det(A) is not equal to zero,A is invertible.
#Testing 
###> source('C:/work/Training/datasciencecoursera_git/datasciencecoursera/ProgrammingAssignment2/cachematrix.R')
###> test_matrix<-matrix(c(1,2,3,0,1,5,5,6,0),3,3)
###> m1<-makeCacheMatrix(test_matrix)
###> m1$get()
###     [,1] [,2] [,3]
###[1,]    1    0    5
###[2,]    2    1    6
###[3,]    3    5    0
###> m1$get_inverse()
###NULL
###> cacheSolve(m1)
###     [,1] [,2] [,3]
###[1,] -6.0    5 -1.0
###[2,]  3.6   -3  0.8
###[3,]  1.4   -1  0.2
###> cacheSolve(m1)
###geting cache data
###     [,1] [,2] [,3]
###[1,] -6.0    5 -1.0
###[2,]  3.6   -3  0.8
###[3,]  1.4   -1  0.2
###> m1$get_inverse()
###     [,1] [,2] [,3]
###[1,] -6.0    5 -1.0
###[2,]  3.6   -3  0.8
###[3,]  1.4   -1  0.2