rm(list=ls())

solveEquation<-function(A,x=NULL,b=NULL){
  
  b_test<-matrix(0,ncol(A),1)
  r<-ncol(A)
  b_2<-matrix(0,ncol(A),1)
  
  if(is.null(b)){
    for(i in 1:r){
      for(j in 1:r){
        b_2[i]=b_2[i]+A[i,j]*x[j]
      }
    }
    print("행렬b는:")
    return (b_2)
  }
  
  else if(is.null(x)){
    
    det(A)
    a<-c()
    if(ncol(A)==2){
      a[1]<-det(matrix(c(b,A[,2]),2,2))
      a[2]<-det(matrix(c(A[,1],b),2,2))
    }
    else if(ncol(A)==3){
      a[1]<-det(matrix(c(b,A[,2],A[,3]),3,3))
      a[2]<-det(matrix(c(A[,1],b,A[,3]),3,3))
      a[3]<-det(matrix(c(A[,1],A[,2],b),3,3))
    }
    else if(ncol(A)==4){
      a[1]<-det(matrix(c(b,A[,2],A[,3],A[,4]),4,4))
      a[2]<-det(matrix(c(A[,1],b,A[,3],A[,4]),4,4))
      a[3]<-det(matrix(c(A[,1],A[,2],b,A[,4]),4,4))
      a[4]<-det(matrix(c(A[,1],A[,2],A[,3],b),4,4))
    }
    
    for(i in 1:ncol(A)){
    x[i]=a[i]/det(A)
    }
    print("행렬x는:")
    return (x)
  }
  
  else{
    for(i in 1:r){
      for(j in 1:r){
        b_test[i]=b_test[i]+A[i,j]*x[j]
      }
    }
    if (all.equal(b,b_test)=='TRUE') {return ('Correct')
    }else {return ('Incorrect')}
  }
}

#A,b가 들어가면 x가 반환
A<-matrix(c(1,2,-3,2,-1,4,-1,0,5),3,3,byrow=TRUE)
b<-matrix(c(-3,5,10),3,1)
x<-NULL
solveEquation(A,x,b)

#A,x가 들어가면 b를 반환
A<-matrix(c(1,2,-3,2,-1,4,-1,0,5),3,3,byrow=TRUE)
x<-matrix(c(-0.5,1.6,1.9),3,1)
b<-NULL
solveEquation(A,x,b)

#A,x,b가 들어가면 correct 반환
A<-matrix(c(1,2,-3,2,-1,4,-1,0,5),3,3,byrow=TRUE)
x<-matrix(c(-0.5,1.6,1.9),3,1)
b<-matrix(c(-3,5,10),3,1)
solveEquation(A,x,b)

#2차 행렬일 때 A,b가 들어가면 x가 반환
A<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
b<-matrix(c(5,11),2,1)
x<-NULL
solveEquation(A,x,b)

#2차 행렬일 때 A,x가 들어가면 b를 반환
A<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
x<-matrix(c(1,2),2,1)
b<-NULL
solveEquation(A,x,b)

#2차 행렬일 때 A,x,b가 들어가면 correct 반환
A<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
x<-matrix(c(1,2),2,1)
b<-matrix(c(5,11),2,1)
solveEquation(A,x,b)

#4차 행렬일 때 A,b가 들어가면 x가 반환
A<-matrix(c(1,0,0,1,-1,1,0,2,0,1,0,1,0,0,1,1),4,4,byrow=TRUE)
b<-matrix(c(2,1,2,1),4,1)
x<-NULL
solveEquation(A,x,b)

#4차 행렬일 때 A,x가 들어가면 b를 반환
A<-matrix(c(1,0,0,1,-1,1,0,2,0,1,0,1,0,0,1,1),4,4,byrow=TRUE)
x<-matrix(c(1,1,0,0),4,1)
b<-NULL
solveEquation(A,x,b)

#4차 행렬일 때 A,x,b가 들어가면 correct 반환
A<-matrix(c(1,0,0,1,-1,1,0,2,0,1,0,1,0,0,1,1),4,4,byrow=TRUE)
x<-matrix(c(1,1,0,0),4,1)
b<-matrix(c(2,1,2,1),4,1)
solveEquation(A,x,b)

