this function creates a matrix than cache its inverse

makeCachematrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

This function computes the inverse of the matrix returned by the function above


cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting catched data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
