#1번문제
rm(list=ls())

craps <- function(){  
  first<-sum(sample(1:6,2,replace = T))
  if (first==7|first==11){
    return (1)
  }else if (first==2|first==3|first==12){
    return (0)
  }else {
    for(i in 1:100) {
      sec<-sum(sample(1:6,2,replace = T))
      if (sec==7) return (0)
      if (sec==first) return (1)
    }
  }
}

sum=0
for(i in 1:1000){
  sum=craps()+sum
}
#(a) CRAPS게임의 승률
sum/1000

winner<-function(ky,sy){
  while(sy!=0&ky!=0){
    if (craps()==1){
      sy=sy-1
      ky=ky+1
    }else{
      ky=ky-1
      sy=sy+1
    }
    
    if(ky==0){ 
      return (0)
    }else if (sy==0){ 
      return (1)}
  }  
}

#(b) 조건이 주어졌을 때 KY의 승률
sum2 <- sum(unlist(replicate(1000, winner(12,9))))
sum2/1000
sum3 <- sum(unlist(replicate(1000, winner(20,9))))
sum3/1000

#2번 문제
Mat1 <- matrix(c(1, 0, 1, 1, 1,
                 0, 0, 0, 1, 1,                 
                 0, 1, 1, 1, 1,                 
                 0, 1, 1, 1, 1,                 
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE) 
Mat1
Mat2 <- matrix(c(1, 0, 1, 1, 1,                  
                 1, 1, 1, 1, 1,                  
                 0, 1, 1, 1, 1,                  
                 0, 1, 1, 1, 1,                  
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE) 
k=0
Largest<-function(matrix){
  n<-nrow(matrix)
  for (z in n:1){
    for(i in 1:(n-z+1)){
      for(j in 1:(n-z+1)){
        k=sum(matrix[i:(i+z-1),j:(j+z-1)])
        if(k==z^2) return (z^2)
      }
    }
  }
}

Largest(Mat1)
Largest(Mat2)

#3번 문제
rm(list=ls())
input<-c(10,20,5,30,15)

chopchop<-function(input){
  sample<-list()  #5개 중에 연속된 3개의 숫자를 뽑아 둔 sample
  input2<-list()  #input에서 연속된 3개의 숫자 중 가운데 숫자를 뺀 4개 숫자로 이루어진 input2
  sample2<-list() #input2에서 연속된 3개의 숫자를 뽑아 둔 sample2
  input3<-list()  #위에서 연속된 3개의 숫자 중 가운데 숫자를 빼고 다시 3개로 이루어진 input3
  price<-c()
  last<-c()
  
  for(i in 1:3){
    sample<-rbind(sample, c(input[i],input[i+1],input[i+2]))
    sample<-rbind(sample, c(input[i],input[i+1],input[i+2]))
    input2<-rbind(input2, c(input[-(i+1)]))
    sample2<-rbind(sample2, c(input2[i,1],input2[i,2],input2[i,3]))
    sample2<-rbind(sample2, c(input2[i,2],input2[i,3],input2[i,4]))
    input3<-rbind(input3,c(input2[i,1],input2[i,3],input2[i,4]))
    input3<-rbind(input3,c(input2[i,1],input2[i,2],input2[i,4]))
  }
  
  total<-cbind(sample,sample2,input3)  #총 경우의 수
  for(i in 1:6){
    price<-unlist(total[i,])
    last[i]<-price[1]*price[2]*price[3]+price[4]*price[5]*price[6]+price[7]*price[8]*price[9]
  }
  min(last)
}

chopchop(input)

#4번 문제
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